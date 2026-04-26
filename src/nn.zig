const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");

pub const SCALE = 400;
const QA: i16 = 255;
const QB: i16 = 64;

const InputSize = 768;
const AccumSize = 512;

const Buckets = 8;
const BucketDivisor = std.math.divCeil(comptime_int, 32, Buckets) catch unreachable;

const Net = extern struct {
    acc_weights: [InputSize][AccumSize]i16,
    acc_biases: [AccumSize]i16,
    out_weights: [Buckets][2 * AccumSize]i16,
    out_bias: [Buckets]i16,
};
const Info = struct {
    move: tp.Move,
    moving: tp.PieceType,
    cap: ?tp.PieceType,
    side: bo.Side,
};
pub const NN = struct {
    accum_w: [AccumSize]i16,
    accum_b: [AccumSize]i16,
    infos: [2048]Info,
    pos: u12,
    lazy_pos: u12,

    network: Net,

    pub fn init(comptime folder: anytype, comptime net: anytype) !NN {
        const file = try std.fs.cwd().openFile(folder ++ net, .{ .mode = .read_only });
        defer file.close();

        var buf: [@sizeOf(Net)]u8 = undefined;
        var f_reader = file.reader(&buf);
        var reader = &f_reader.interface;

        return .{
            .accum_w = std.mem.zeroes([AccumSize]i16),
            .accum_b = std.mem.zeroes([AccumSize]i16),
            .infos = std.mem.zeroes([2048]Info),
            .pos = 0,
            .lazy_pos = 0,
            .network = try reader.takeStruct(Net, .little),
        };
    }

    // Squared Clipped ReLU (SCReLU)
    inline fn activate(i: i16) i32 {
        const clamp: i32 = @intCast(std.math.clamp(i, 0, QA));
        return clamp * clamp;
    }

    inline fn calcIndex(
        perspective: bo.Side,
        sq: tp.Square,
        piece: tp.PieceType,
        side: bo.Side,
    ) usize {
        const piece_in: usize = @intCast(@intFromEnum(piece));
        var side_in: usize = @intCast(@intFromEnum(side));
        var sq_in: usize = @intCast(@intFromEnum(sq));
        if (perspective == .Black) {
            side_in = 1 - side_in;
            sq_in ^= 0b111000;
        }

        return side_in * 6 * 64 + piece_in * 64 + sq_in;
    }

    inline fn accumAdd(
        self: *NN,
        sq: tp.Square,
        typ: tp.PieceType,
        side: bo.Side,
    ) void {
        const index_w = calcIndex(.White, sq, typ, side);
        const index_b = calcIndex(.Black, sq, typ, side);

        for (0..AccumSize) |i| {
            self.accum_w[i] += self.network.acc_weights[index_w][i];
            self.accum_b[i] += self.network.acc_weights[index_b][i];
        }
    }

    inline fn accumSub(
        self: *NN,
        sq: tp.Square,
        typ: tp.PieceType,
        side: bo.Side,
    ) void {
        const index_w = calcIndex(.White, sq, typ, side);
        const index_b = calcIndex(.Black, sq, typ, side);

        for (0..AccumSize) |i| {
            self.accum_w[i] -= self.network.acc_weights[index_w][i];
            self.accum_b[i] -= self.network.acc_weights[index_b][i];
        }
    }

    inline fn accumAddSub(
        self: *NN,
        from: tp.Square,
        to: tp.Square,
        typ: tp.PieceType,
        side: bo.Side,
    ) void {
        const index_wf = calcIndex(.White, from, typ, side);
        const index_bf = calcIndex(.Black, from, typ, side);
        const index_wt = calcIndex(.White, to, typ, side);
        const index_bt = calcIndex(.Black, to, typ, side);

        for (0..AccumSize) |i| {
            self.accum_w[i] -= self.network.acc_weights[index_wf][i];
            self.accum_b[i] -= self.network.acc_weights[index_bf][i];
            self.accum_w[i] += self.network.acc_weights[index_wt][i];
            self.accum_b[i] += self.network.acc_weights[index_bt][i];
        }
    }

    inline fn accumAddSubProm(
        self: *NN,
        from: tp.Square,
        to: tp.Square,
        from_typ: tp.PieceType,
        to_typ: tp.PieceType,
        side: bo.Side,
    ) void {
        const index_wf = calcIndex(.White, from, from_typ, side);
        const index_bf = calcIndex(.Black, from, from_typ, side);
        const index_wt = calcIndex(.White, to, to_typ, side);
        const index_bt = calcIndex(.Black, to, to_typ, side);

        for (0..AccumSize) |i| {
            self.accum_w[i] -= self.network.acc_weights[index_wf][i];
            self.accum_b[i] -= self.network.acc_weights[index_bf][i];
            self.accum_w[i] += self.network.acc_weights[index_wt][i];
            self.accum_b[i] += self.network.acc_weights[index_bt][i];
        }
    }

    pub inline fn inputAccum(self: *NN, b: *const bo.Board) void {
        self.pos = 0;
        self.lazy_pos = 0;

        self.accum_w = std.mem.zeroes([AccumSize]i16);
        self.accum_b = std.mem.zeroes([AccumSize]i16);

        var iter = b.w_pieces;
        while (iter.popLsb()) |sq| self.accumAdd(sq, b.pieceType(sq), .White);

        iter = b.b_pieces;
        while (iter.popLsb()) |sq| self.accumAdd(sq, b.pieceType(sq), .Black);

        for (0..AccumSize) |i| {
            self.accum_w[i] += self.network.acc_biases[i];
            self.accum_b[i] += self.network.acc_biases[i];
        }
    }

    inline fn doApply(self: *NN, inf: Info) void {
        switch (inf.move.typ) {
            .Normal => {
                self.accumAddSub(inf.move.from, inf.move.to, inf.moving, inf.side);
                if (inf.cap) |t| self.accumSub(inf.move.to, t, inf.side.getOther());
            },
            .EnPassant => {
                self.accumAddSub(inf.move.from, inf.move.to, .Pawn, inf.side);
                if (inf.side == .Black)
                    self.accumSub(inf.move.to.getApply(.North), .Pawn, inf.side.getOther())
                else
                    self.accumSub(inf.move.to.getApply(.South), .Pawn, inf.side.getOther());
            },
            .CastleKingside => {
                self.accumAddSub(inf.move.from, inf.move.to, .King, inf.side);
                if (inf.side == .Black)
                    self.accumAddSub(.h8, .f8, .Rook, .Black)
                else
                    self.accumAddSub(.h1, .f1, .Rook, .White);
            },
            .CastleQueenside => {
                self.accumAddSub(inf.move.from, inf.move.to, .King, inf.side);
                if (inf.side == .Black)
                    self.accumAddSub(.a8, .d8, .Rook, .Black)
                else
                    self.accumAddSub(.a1, .d1, .Rook, .White);
            },
            .PromKnight => {
                self.accumAddSubProm(inf.move.from, inf.move.to, .Pawn, .Knight, inf.side);
                if (inf.cap) |t| self.accumSub(inf.move.to, t, inf.side.getOther());
            },
            .PromBishop => {
                self.accumAddSubProm(inf.move.from, inf.move.to, .Pawn, .Bishop, inf.side);
                if (inf.cap) |t| self.accumSub(inf.move.to, t, inf.side.getOther());
            },
            .PromRook => {
                self.accumAddSubProm(inf.move.from, inf.move.to, .Pawn, .Rook, inf.side);
                if (inf.cap) |t| self.accumSub(inf.move.to, t, inf.side.getOther());
            },
            .PromQueen => {
                self.accumAddSubProm(inf.move.from, inf.move.to, .Pawn, .Queen, inf.side);
                if (inf.cap) |t| self.accumSub(inf.move.to, t, inf.side.getOther());
            },
        }
    }

    inline fn doRemove(self: *NN, inf: Info) void {
        switch (inf.move.typ) {
            .Normal => {
                self.accumAddSub(inf.move.to, inf.move.from, inf.moving, inf.side);
                if (inf.cap) |t| self.accumAdd(inf.move.to, t, inf.side.getOther());
            },
            .EnPassant => {
                self.accumAddSub(inf.move.to, inf.move.from, .Pawn, inf.side);
                if (inf.side == .Black)
                    self.accumAdd(inf.move.to.getApply(.North), .Pawn, inf.side.getOther())
                else
                    self.accumAdd(inf.move.to.getApply(.South), .Pawn, inf.side.getOther());
            },
            .CastleKingside => {
                self.accumAddSub(inf.move.to, inf.move.from, .King, inf.side);
                if (inf.side == .Black)
                    self.accumAddSub(.f8, .h8, .Rook, .Black)
                else
                    self.accumAddSub(.f1, .h1, .Rook, .White);
            },
            .CastleQueenside => {
                self.accumAddSub(inf.move.to, inf.move.from, .King, inf.side);
                if (inf.side == .Black)
                    self.accumAddSub(.d8, .a8, .Rook, .Black)
                else
                    self.accumAddSub(.d1, .a1, .Rook, .White);
            },
            .PromKnight => {
                self.accumAddSubProm(inf.move.to, inf.move.from, .Knight, .Pawn, inf.side);
                if (inf.cap) |t| self.accumAdd(inf.move.to, t, inf.side.getOther());
            },
            .PromBishop => {
                self.accumAddSubProm(inf.move.to, inf.move.from, .Bishop, .Pawn, inf.side);
                if (inf.cap) |t| self.accumAdd(inf.move.to, t, inf.side.getOther());
            },
            .PromRook => {
                self.accumAddSubProm(inf.move.to, inf.move.from, .Rook, .Pawn, inf.side);
                if (inf.cap) |t| self.accumAdd(inf.move.to, t, inf.side.getOther());
            },
            .PromQueen => {
                self.accumAddSubProm(inf.move.to, inf.move.from, .Queen, .Pawn, inf.side);
                if (inf.cap) |t| self.accumAdd(inf.move.to, t, inf.side.getOther());
            },
        }
    }

    // This should be called after actually applying the move
    pub inline fn apply(self: *NN, b: *const bo.Board, move: tp.Move, undo: tp.Remove) void {
        if (self.lazy_pos < self.pos) self.doLazy();

        self.infos[self.lazy_pos] = .{
            .move = move,
            .moving = switch (move.typ) {
                .Normal => b.pieceType(move.to),
                .EnPassant, .PromKnight, .PromBishop, .PromRook, .PromQueen => .Pawn,
                .CastleKingside, .CastleQueenside => .King,
            },
            .cap = undo.typ,
            .side = b.side.getOther(),
        };
        self.lazy_pos += 1;
    }

    pub inline fn remove(self: *NN) void {
        self.lazy_pos -= 1;
    }

    inline fn doLazy(self: *NN) void {
        if (self.pos < self.lazy_pos) {
            for (self.pos..self.lazy_pos) |j| self.doApply(self.infos[j]);
        } else if (self.lazy_pos < self.pos) {
            for (self.lazy_pos..self.pos) |j| self.doRemove(self.infos[j]);
        }
        self.pos = self.lazy_pos;
    }

    inline fn chooseBucket(b: *const bo.Board) usize {
        const piece_count = b.w_pieces.op_or(b.b_pieces).popcount() - 2;

        return std.math.clamp(@divFloor(piece_count, BucketDivisor), 0, Buckets - 1);
    }

    pub inline fn output(self: *NN, b: *const bo.Board) i32 {
        self.doLazy();

        var ret: i32 = 0;
        const bucket = chooseBucket(b);

        const weights = &self.network.out_weights[bucket];
        if (b.side == .White) {
            for (0..AccumSize) |i| {
                ret += activate(self.accum_w[i]) * @as(i32, @intCast(weights[i]));
                ret += activate(self.accum_b[i]) * @as(i32, @intCast(weights[i + AccumSize]));
            }
        } else {
            for (0..AccumSize) |i| {
                ret += activate(self.accum_w[i]) * @as(i32, @intCast(weights[i + AccumSize]));
                ret += activate(self.accum_b[i]) * @as(i32, @intCast(weights[i]));
            }
        }
        ret = @divTrunc(ret, QA);
        ret += @intCast(self.network.out_bias[bucket]);

        ret *= SCALE;
        ret = @divTrunc(ret, QA * QB);
        return ret;
    }
};
