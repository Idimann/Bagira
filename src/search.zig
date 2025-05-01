const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const mv = @import("movegen.zig");
const ev = @import("eval.zig");

const MateVal = std.math.maxInt(i32);
pub inline fn gameEnd(
    b: *const bo.Board,
    mvs: usize,
    checks: usize,
) ?i32 {
    // Check and stalemate
    if (mvs == 0) return if (checks > 0) -MateVal else 0;

    // Insufficient material
    const w_pop = b.w_pieces.popcount();
    var w_scen = w_pop <= 2;
    if (w_scen) {
        if (b.w_pieces.op_and(b.lines).v > 0 or b.w_pieces.op_and(b.pawns).v > 0)
            w_scen = false
        else {
            var b_scen = b.b_pieces.popcount() <= 2;
            if (b_scen) {
                if (b.b_pieces.op_and(b.lines).v > 0 or b.b_pieces.op_and(b.pawns).v > 0)
                    b_scen = false;
            }
            if (w_scen and b_scen) return 0;
        }
    } else if (w_pop == 3 and b.b_pieces.popcount() == 1) { //2 knights vs nothing
        if (b.w_pieces.op_and(b.pawns.op_or(b.diags).op_or(b.lines)).v == 0)
            return 0;
    }

    return null;
}

pub inline fn historyDependentDraw(
    b: *const bo.Board,
    hash: u64,
) bool {
    // 50 move rule
    if (b.move_rule >= 100) return true;

    // Repetition
    if (b.hash_in > 1) {
        var iter: u8 = 1;

        while (iter < b.move_rule) {
            if (b.hash[b.hash_in - iter] == hash) return true;

            iter += 2;
        }
    }

    return false;
}

pub const MaxDepth = 256;

// We don't check for game ends in this, cause of need to gen all moves to check it
fn quietSearch(b: *bo.Board, alpha: i32, beta: i32, alloc: std.mem.Allocator) !i32 {
    if (historyDependentDraw(b, b.hash[b.hash_in])) return 0;

    var al = @max(alpha, ev.eval(b) * MaxDepth);

    var gen = mv.Maker.init(b);

    // Don't use static eval if in check
    if (gen.checks > 0) al = alpha;

    if (al >= beta) return al;

    var list = std.ArrayList(tp.Move).init(alloc);
    defer list.deinit();
    try gen.gen(&list, .Capture);

    for (list.items) |mov| {
        const undo = b.apply(mov);
        const score = -(try quietSearch(b, -beta, -al, alloc));
        try b.remove(mov, undo);

        if (score > al) {
            al = score;
            if (score >= beta) break;
        }
    }

    // We can do this cause of the max depth multiply
    if (al > 0) al -= 1;
    if (al < 0) al += 1;

    return al;
}

const NodeType = enum(u2) { Pv, Cut, All };
const TransEntry = packed struct {
    hash: u64,
    typ: NodeType,
    val: i32,
    dep: u8,
    time: u8,
    has_move: bool,
    move: tp.Move,
    check: u64,

    inline fn getCheck(self: *const TransEntry) u64 {
        return (@as(u64, @as(u32, @bitCast(self.val))) *% @as(u64, @intCast(self.dep))) ^
            @as(u64, @as(u15, @bitCast(self.move)));
    }

    inline fn resetCheck(self: *TransEntry) void {
        self.check = self.hash ^ self.getCheck();
    }
};
const TransSize = (32 * (1 << 20)) / @sizeOf(TransEntry) - 1;
var TransTable = std.mem.zeroes([TransSize]TransEntry);

const TransReturn = packed struct {
    en: *TransEntry,
    val: TransEntry,
    fine: bool,
    half_fine: bool,
};
pub inline fn getTrans(hash: u64) TransReturn {
    const ret = &TransTable[hash % TransSize];
    var val = ret.*;

    if (val.check != val.hash ^ val.getCheck()) {
        val = std.mem.zeroes(TransEntry);
        ret.* = val;
    }
    if (val.hash != hash) {
        return .{
            .en = ret,
            .val = val,
            .fine = false,
            .half_fine = val.check == val.hash ^ val.getCheck() and val.hash != 0,
        };
    }

    return .{
        .en = ret,
        .val = val,
        .fine = true,
        .half_fine = true,
    };
}

var History = std.mem.zeroes([12][64]i32);
var EndTime: i64 = std.math.maxInt(i64);
var Ended = false;

pub fn setupSearch() void {
    History = std.mem.zeroes([12][64]i32);
    EndTime = std.math.maxInt(i64);
    Ended = false;
}

const MoveStage = enum {
    // Hash move
    Trans,

    // Captures
    CapturesGen,
    Captures,

    // Quiets
    QuietsGen,
    Quiets,

    // Castles
    CastleGen,
    Castle,
};
fn nextMove(
    stage: *MoveStage,
    gen: *mv.Maker,
    list: *std.ArrayList(tp.Move),
    table: *const TransReturn,
) !?tp.Move {
    switch (stage.*) {
        .Trans => {
            stage.* = .CapturesGen;
            if (table.fine and table.val.has_move) return table.val.move;
        },
        .CapturesGen => {
            stage.* = .Captures;
            try gen.gen(list, .Capture);
        },
        .Captures => {
            var best: i32 = 0;
            var in: usize = 0;

            for (0..list.items.len) |i| {
                const check = list.items[i].to;

                var score: i32 = 0;
                if (gen.b.pawns.check(check))
                    score = ev.PawnBase
                else if (gen.b.diags.check(check)) {
                    score = if (gen.b.lines.check(check))
                        ev.QueenBase
                    else
                        ev.BishopBase;
                } else if (gen.b.lines.check(check))
                    score = ev.RookBase
                else
                    score = ev.KnightBase;

                if (score > best) {
                    best = score;
                    in = i;

                    if (best == ev.QueenBase) break;
                }
            }

            if (best != 0) return list.swapRemove(in);
            stage.* = .QuietsGen;
        },
        .QuietsGen => {
            stage.* = .Quiets;
            try gen.gen(list, .Quiet);
        },
        .Quiets => {
            var best: i32 = 0;
            var in: usize = 0;

            const add: u8 = if (gen.b.side == .White) 0 else 6;
            for (0..list.items.len) |i| {
                const check = list.items[i].from;
                const to = @intFromEnum(list.items[i].to);

                var score: i32 = 0;
                if (gen.b.pawns.check(check))
                    score = History[0 + add][to]
                else if (gen.b.diags.check(check)) {
                    score = if (gen.b.lines.check(check))
                        History[4 + add][to]
                    else
                        History[2 + add][to];
                } else if (gen.b.lines.check(check))
                    score = History[3 + add][to]
                else if (check == gen.b.w_king or check == gen.b.b_king)
                    score = History[5 + add][to]
                else
                    score = History[1 + add][to];

                if (score > best) {
                    best = score;
                    in = i;
                }
            }

            if (best != 0) return list.swapRemove(in);
            stage.* = .CastleGen;
        },
        .CastleGen => {
            stage.* = .Castle;
            try gen.gen(list, .Castle);
        },
        .Castle => {
            return list.popOrNull();
        },
    }

    return nextMove(stage, gen, list, table);
}

pub fn search(
    b: *bo.Board,
    alpha: i32,
    beta: i32,
    dep: u8,
    ply: u8,
    time: u8,
    alloc: std.mem.Allocator,
) !i32 {
    if (Ended) return 0;

    const hash = b.hash[b.hash_in];

    // Check for 50 move rule and repetition
    if (historyDependentDraw(b, hash)) return 0;

    const table = getTrans(hash);

    if (b.move_rule < 90 and table.fine and table.val.dep >= dep) {
        switch (table.val.typ) {
            .Pv => {
                return table.val.val;
            },
            .All => {
                if (table.val.val <= alpha) return table.val.val;
            },
            .Cut => {
                if (table.val.val >= beta) return table.val.val;
            },
        }
    }

    const table_value =
        if (table.half_fine)
        (@as(i32, table.val.dep) -
            8 * @as(i32, time - table.val.time)) *
            2 + @as(i32, if (table.val.typ == .Pv) 1 else 0)
    else
        0;

    if (std.time.milliTimestamp() > EndTime) {
        Ended = true;
        return 0;
    }

    // Search end
    if (dep == 0) {
        const ret = try quietSearch(b, alpha, beta, alloc);

        const typ: NodeType = if (ret >= beta) .Cut else if (ret < alpha) .All else .Pv;
        if (@as(i32, if (typ == .Pv) 1 else 0) > table_value) {
            table.en.hash = hash;
            table.en.val = ret;
            table.en.dep = 0;
            table.en.time = time;
            table.en.has_move = false;
            table.en.move = std.mem.zeroes(tp.Move);
            table.en.typ = typ;
            table.en.resetCheck();
        }
        return ret;
    }

    var al = alpha;
    var ret: i32 = -MateVal;
    var move: ?tp.Move = null;

    var gen = mv.Maker.init(b);
    var list = std.ArrayList(tp.Move).init(alloc);
    defer list.deinit();

    var stage: MoveStage = .Trans;
    var do_null = false;

    while (try nextMove(&stage, &gen, &list, &table)) |mov| {
        const undo = b.apply(mov);

        const next = dep - 1;
        // LMR
        // if (gen.checks == 0 and al > alpha and (!table.fine or table.val.typ != .Pv)) {
        //     if (ply >= 6) {
        //         next -= @divTrunc(ply, 3);
        //     } else if (ply >= 3) {
        //         next -= 1;
        //     }
        // }

        var score: i32 = undefined;
        if (do_null) {
            score = -(try search(b, -al - MaxDepth, -al, next, ply + 1, time, alloc));
            if (score > al and @as(i33, beta) - @as(i33, al) > MaxDepth) {
                score = -(try search(b, -beta, -al, next, ply + 1, time, alloc));

                // if (score > al)
                //     score = -(try search(b, -beta, -al, dep - 1, ply + 1, time, alloc));
            }
        } else score = -(try search(b, -beta, -al, dep - 1, ply + 1, time, alloc));

        try b.remove(mov, undo);

        if (score > ret or move == null) {
            do_null = true;
            ret = score;
            move = mov;
            if (score > al) al = score;
            if (score >= beta) {
                // Update history (only on quiet moves)
                if (undo.typ == null) {
                    const add: u4 = if (b.side == .White) 0 else 6;

                    if (b.pawns.check(mov.from))
                        History[0 + add][@intFromEnum(mov.to)] += dep * dep
                    else if (b.diags.check(mov.from)) {
                        if (b.lines.check(mov.from))
                            History[4 + add][@intFromEnum(mov.to)] += dep * dep
                        else
                            History[2 + add][@intFromEnum(mov.to)] += dep * dep;
                    } else if (b.lines.check(mov.from))
                        History[3 + add][@intFromEnum(mov.to)] += dep * dep
                    else if (b.w_king == mov.from or b.b_king == mov.from)
                        History[5 + add][@intFromEnum(mov.to)] += dep * dep
                    else
                        History[1 + add][@intFromEnum(mov.to)] += dep * dep;
                }
                break;
            }
        }
    }

    if (!table.fine or !table.val.has_move) {
        if (gameEnd(b, gen.num, gen.checks)) |end| {
            const typ: NodeType = if (al >= beta) .Cut else if (al < alpha) .All else .Pv;
            if (@as(i32, 255) * 2 + @as(i32, if (typ == .Pv) 1 else 0) > table_value) {
                table.en.hash = hash;
                table.en.val = end;
                table.en.dep = 255;
                table.en.time = time;
                table.en.has_move = false;
                table.en.move = std.mem.zeroes(tp.Move);
                table.en.typ = typ;
                table.en.resetCheck();
            }
            return end;
        }
    }

    // We can do this cause of the max depth multiply
    if (ret > 0) ret -= 1;
    if (ret < 0) ret += 1;

    // If this is a null window search, we shouldn't input into the trans table
    const typ: NodeType = if (ret >= beta) .Cut else if (ret <= alpha) .All else .Pv;
    if (@as(i32, dep) * 2 + @as(i32, if (typ == .Pv) 1 else 0) > table_value) {
        table.en.hash = hash;
        table.en.val = ret;
        table.en.dep = dep;
        table.en.time = time;
        if (move) |mov| {
            table.en.has_move = true;
            table.en.move = mov;
        }
        table.en.typ = typ;
        table.en.resetCheck();
    }
    return ret;
}

pub fn searchPrint(b: *bo.Board, dep: u8, time: u8, alloc: std.mem.Allocator) !void {
    var list = std.ArrayList(tp.Move).init(alloc);
    _ = try mv.gen(b, &list);

    var alpha: i32 = -MateVal;
    var move: ?tp.Move = null;

    for (list.items) |mov| {
        const undo = b.apply(mov);
        var res = -(try search(b, -MateVal, -alpha, dep - 1, time, alloc));
        res = @divFloor(res, MaxDepth);
        mov.print();
        std.debug.print(": {}\n", .{res});
        try b.remove(mov, undo);

        if (res > alpha) {
            alpha = res;
            move = mov;
        }
    }
    std.debug.print("Best: {}\n", .{alpha});

    if (move) |mov| {
        mov.print();
        std.debug.print(" with hash: {}\n", .{b.hash()});
        const undo = b.apply(mov);
        try printPv(b);
        try b.remove(mov, undo);
    }

    list.deinit();
}

pub fn printTransData() void {
    var empty: usize = 0;
    var full: usize = 0;

    for (0..TransSize) |i| {
        if (TransTable[i].hash != 0)
            full += 1
        else
            empty += 1;
    }

    std.debug.print("Empty: {}, Full: {}\n", .{ empty, full });
}

pub fn printPv(b: *bo.Board) !void {
    const hash = b.hash[b.hash_in];
    const table = getTrans(hash);
    if (!table.fine or !table.val.has_move) {
        std.debug.print("Ended pv sequence\n", .{});
        return;
    }

    table.val.move.print();
    std.debug.print(" with depth {}\n", .{table.val.dep});

    const undo = b.apply(table.val.move);
    try printPv(b);
    try b.remove(table.val.move, undo);
}
