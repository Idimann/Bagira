const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");

pub const SCALE: i32 = 400;
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
pub const NN = struct {
    accum_w: [AccumSize]i16,
    accum_b: [AccumSize]i16,

    network: Net,

    pub fn initEmpty() NN {
        return std.mem.zeroes(NN);
    }

    pub fn init(comptime folder: anytype, comptime net: anytype) !NN {
        const file = try std.fs.cwd().openFile(folder ++ net, .{ .mode = .read_only });
        defer file.close();

        var buf_reader = std.io.bufferedReader(file.reader());
        var reader = buf_reader.reader();

        return .{
            .accum_w = std.mem.zeroes([AccumSize]i16),
            .accum_b = std.mem.zeroes([AccumSize]i16),
            .network = try reader.readStructEndian(Net, .little),
        };
    }

    // Squared Clipped ReLU (SCReLU)
    inline fn activation(i: i16) i32 {
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

    inline fn accumAdd(self: *NN, sq: tp.Square, typ: tp.PieceType, side: bo.Side) void {
        const index_w = calcIndex(.White, sq, typ, side);
        const index_b = calcIndex(.Black, sq, typ, side);

        for (0..AccumSize) |i| {
            self.accum_w[i] += self.network.acc_weights[index_w][i];
            self.accum_b[i] += self.network.acc_weights[index_b][i];
        }
    }

    inline fn accumSub(self: *NN, sq: tp.Square, typ: tp.PieceType, side: bo.Side) void {
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

    pub inline fn inputAccum(self: *NN, b: *const bo.Board) void {
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

    // This should be called after the move has already been made
    pub inline fn move(self: *NN, b: *const bo.Board, m: tp.Move, undo: tp.Remove) void {
        switch (m.typ) {
            .Normal => {
                const typ = b.pieceType(m.to);
                self.accumAddSub(m.from, m.to, typ, b.side.getOther());
                if (undo.typ) |t| self.accumSub(m.to, t, b.side);
            },
            .EnPassant => {
                const typ = b.pieceType(m.to);
                self.accumAddSub(m.from, m.to, typ, b.side.getOther());
                if (b.side == .White)
                    self.accumSub(m.to.getApply(.North), .Pawn, b.side)
                else
                    self.accumSub(m.to.getApply(.South), .Pawn, b.side);
            },
            .CastleKingside => {
                const typ = b.pieceType(m.to);
                self.accumAddSub(m.from, m.to, typ, b.side.getOther());
                if (b.side == .White) {
                    self.accumSub(.h8, .Rook, .Black);
                    self.accumAdd(.f8, .Rook, .Black);
                } else {
                    self.accumSub(.h1, .Rook, .White);
                    self.accumAdd(.f1, .Rook, .White);
                }
            },
            .CastleQueenside => {
                const typ = b.pieceType(m.to);
                self.accumAddSub(m.from, m.to, typ, b.side.getOther());
                if (b.side == .White) {
                    self.accumSub(.a8, .Rook, .Black);
                    self.accumAdd(.d8, .Rook, .Black);
                } else {
                    self.accumSub(.a1, .Rook, .White);
                    self.accumAdd(.d1, .Rook, .White);
                }
            },
            .PromKnight => {
                const typ = b.pieceType(m.to);
                self.accumSub(m.from, typ, b.side.getOther());
                self.accumAdd(m.to, .Knight, b.side.getOther());
                if (undo.typ) |t| self.accumSub(m.to, t, b.side);
            },
            .PromBishop => {
                const typ = b.pieceType(m.to);
                self.accumSub(m.from, typ, b.side.getOther());
                self.accumAdd(m.to, .Bishop, b.side.getOther());
                if (undo.typ) |t| self.accumSub(m.to, t, b.side);
            },
            .PromRook => {
                const typ = b.pieceType(m.to);
                self.accumSub(m.from, typ, b.side.getOther());
                self.accumAdd(m.to, .Rook, b.side.getOther());
                if (undo.typ) |t| self.accumSub(m.to, t, b.side);
            },
            .PromQueen => {
                const typ = b.pieceType(m.to);
                self.accumSub(m.from, typ, b.side.getOther());
                self.accumAdd(m.to, .Queen, b.side.getOther());
                if (undo.typ) |t| self.accumSub(m.to, t, b.side);
            },
        }
    }

    // This should be called before the move gets removed
    pub inline fn remove(self: *NN, b: *const bo.Board, m: tp.Move, undo: tp.Remove) void {
        switch (m.typ) {
            .Normal => {
                const typ = b.pieceType(m.to);
                self.accumAdd(m.from, typ, b.side.getOther());
                self.accumSub(m.to, typ, b.side.getOther());
                if (undo.typ) |t| self.accumAdd(m.to, t, b.side);
            },
            .EnPassant => {
                const typ = b.pieceType(m.to);
                self.accumAdd(m.from, typ, b.side.getOther());
                self.accumSub(m.to, typ, b.side.getOther());
                if (b.side == .White)
                    self.accumAdd(m.to.getApply(.North), .Pawn, b.side)
                else
                    self.accumAdd(m.to.getApply(.South), .Pawn, b.side);
            },
            .CastleKingside => {
                const typ = b.pieceType(m.to);
                self.accumAdd(m.from, typ, b.side.getOther());
                self.accumSub(m.to, typ, b.side.getOther());
                if (b.side == .White) {
                    self.accumAdd(.h8, .Rook, .Black);
                    self.accumSub(.f8, .Rook, .Black);
                } else {
                    self.accumAdd(.h1, .Rook, .White);
                    self.accumSub(.f1, .Rook, .White);
                }
            },
            .CastleQueenside => {
                const typ = b.pieceType(m.to);
                self.accumAdd(m.from, typ, b.side.getOther());
                self.accumSub(m.to, typ, b.side.getOther());
                if (b.side == .White) {
                    self.accumAdd(.a8, .Rook, .Black);
                    self.accumSub(.d8, .Rook, .Black);
                } else {
                    self.accumAdd(.a1, .Rook, .White);
                    self.accumSub(.d1, .Rook, .White);
                }
            },
            .PromKnight => {
                const typ = b.pieceType(m.to);
                self.accumAdd(m.from, typ, b.side.getOther());
                self.accumSub(m.to, .Knight, b.side.getOther());
                if (undo.typ) |t| self.accumAdd(m.to, t, b.side);
            },
            .PromBishop => {
                const typ = b.pieceType(m.to);
                self.accumAdd(m.from, typ, b.side.getOther());
                self.accumSub(m.to, .Bishop, b.side.getOther());
                if (undo.typ) |t| self.accumAdd(m.to, t, b.side);
            },
            .PromRook => {
                const typ = b.pieceType(m.to);
                self.accumAdd(m.from, typ, b.side.getOther());
                self.accumSub(m.to, .Rook, b.side.getOther());
                if (undo.typ) |t| self.accumAdd(m.to, t, b.side);
            },
            .PromQueen => {
                const typ = b.pieceType(m.to);
                self.accumAdd(m.from, typ, b.side.getOther());
                self.accumSub(m.to, .Queen, b.side.getOther());
                if (undo.typ) |t| self.accumAdd(m.to, t, b.side);
            },
        }
    }

    inline fn chooseBucket(b: *const bo.Board) usize {
        const piece_count = b.w_pieces.op_or(b.b_pieces).popcount() - 2;

        return std.math.clamp(@divFloor(piece_count, BucketDivisor), 0, Buckets - 1);
    }

    pub inline fn output(self: *NN, b: *const bo.Board) i32 {
        var ret: i32 = 0;
        const bucket = chooseBucket(b);

        for (0..AccumSize) |i| {
            if (b.side == .White) {
                ret += activation(self.accum_w[i]) *
                    @as(i32, @intCast(self.network.out_weights[bucket][i]));
                ret += activation(self.accum_b[i]) *
                    @as(i32, @intCast(self.network.out_weights[bucket][i + AccumSize]));
            } else {
                ret += activation(self.accum_w[i]) *
                    @as(i32, @intCast(self.network.out_weights[bucket][i + AccumSize]));
                ret += activation(self.accum_b[i]) *
                    @as(i32, @intCast(self.network.out_weights[bucket][i]));
            }
        }
        ret = @divTrunc(ret, QA);
        ret += @intCast(self.network.out_bias[bucket]);

        ret *= SCALE;
        ret = @divTrunc(ret, QA * QB);
        return ret;
    }
};
