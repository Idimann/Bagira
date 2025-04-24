const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const mv = @import("movegen.zig");
const ev = @import("eval.zig");

fn better(f: SearchResult, s: SearchResult) bool {
    return f.val > s.val or (f.val == s.val and f.dep < s.dep);
}

fn better_eq(f: SearchResult, s: SearchResult) bool {
    return f.val > s.val or (f.val == s.val and f.dep <= s.dep);
}

pub const SearchResult = struct {
    val: f16, //This has to be between -1 and 1 (-1 <= val <= 1)
    dep: u8,
};
const SearchMax: SearchResult = .{ .val = 1, .dep = 0 };
const SearchMin: SearchResult = .{ .val = -1, .dep = 255 };

inline fn gameEnd(b: *const bo.Board, mvs: usize, checks: usize) ?f16 {
    // Check and stalemate
    if (mvs == 0) return if (checks > 0) -1 else 0;

    //50 move rule
    if (b.move_rule >= 100) return 0;

    if (b.history.items.len > 0) {
        const hash = b.hash();
        //Iterating in reverse should generally be more efficient
        var it = b.history.items.len - 1;
        while (true) {
            if (b.history.items[it] == hash) return 0;
            if (it > 0) it -= 1 else break;
        }
    }

    return null;
}

fn abSearch(
    b: *bo.Board,
    al: SearchResult,
    beta: SearchResult,
    stop: bool,
    alloc: std.mem.Allocator,
) !SearchResult {
    var alpha = al;

    var list = std.ArrayList(tp.Move).init(alloc);
    defer list.deinit();
    const checks = try if (stop) mv.gen_with_checks(b, &list) else mv.gen(b, &list);

    if (gameEnd(b, list.items.len, checks)) |val|
        return .{ .val = val, .dep = 0 }
    else if (stop and checks == 0)
        return .{ .val = ev.eval(b), .dep = 0 };

    var ret = SearchMin;
    for (list.items) |mov| {
        const undo = try b.apply(mov);

        const betNeg = .{ .val = -beta.val, .dep = 255 - beta.dep };
        const alNeg = .{ .val = -alpha.val, .dep = 255 - alpha.dep };

        var val = try abSearch(b, betNeg, alNeg, undo.typ == null, alloc);

        try b.remove(mov, undo);

        val.val = -val.val;
        val.dep += 1;
        if (better(val, ret))
            ret = val;

        if (better(ret, alpha))
            alpha = ret;
        if (better_eq(alpha, beta))
            break;
    }

    return ret;
}

pub fn bestAb(b: *bo.Board, alloc: std.mem.Allocator) !tp.Move {
    var list = std.ArrayList(tp.Move).init(alloc);
    defer list.deinit();
    _ = try mv.gen(b, &list);

    var move: ?tp.Move = null;
    var ret = SearchMin;

    for (list.items) |mov| {
        const undo = try b.apply(mov);

        var val = try abSearch(b, SearchMin, SearchMax, false, alloc);
        val.val = -val.val;
        if (better(val, ret)) {
            move = mov;
            ret = val;
        }

        try b.remove(mov, undo);
    }

    if (move) |m| return m;
    return error.NoMoveAvailable;
}

var Random: std.rand.Xoshiro256 = undefined;

pub fn initRandom() !void {
    Random = std.rand.DefaultPrng.init(blk: {
        var seed: u64 = undefined;
        try std.posix.getrandom(std.mem.asBytes(&seed));
        break :blk seed;
    });
}

pub fn carloSearch(b: *bo.Board, alloc: std.mem.Allocator) !SearchResult {
    var list = std.ArrayList(tp.Move).init(alloc);
    const checks = try mv.gen(b, &list);

    if (gameEnd(b, list.items.len, checks)) |val| {
        list.deinit();
        return .{ .val = val, .dep = 0 };
    }

    var move: ?tp.Move = null;
    var ret = SearchMin;

    for (list.items) |mov| {
        const undo = try b.apply(mov);

        var val = try abSearch(b, SearchMin, SearchMax, false, alloc);

        //Avoiding three fold blindness
        val.val += @as(f16, @floatCast(Random.random().floatNorm(f64))) * 1e-6;

        val.val = -val.val;
        if (better(val, ret)) {
            move = mov;
            ret = val;
        }

        try b.remove(mov, undo);
    }

    if (move) |m| {
        list.deinit();
        const undo = try b.apply(m);
        const returning = try carloSearch(b, alloc);
        try b.remove(m, undo);

        return .{ .val = -returning.val, .dep = returning.dep + 1 };
    }

    return error.NoMoveAvailable;
}
