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

fn abSearch(
    b: *bo.Board,
    al: SearchResult,
    beta: SearchResult,
    dep: u8,
    stop: bool,
    alloc: std.mem.Allocator,
) !SearchResult {
    var alpha = al;

    var list = std.ArrayList(tp.Move).init(alloc);
    defer list.clearAndFree();
    const checks = try mv.gen(b, &list);

    if (list.capacity == 0)
        return .{ .val = if (checks > 0) -1 else 0, .dep = 0 }
    else if (stop and dep == 1 and checks == 0)
        return .{ .val = ev.eval(b), .dep = 0 };

    var ret = SearchMin;
    for (list.items) |mov| {
        const undo = b.apply(mov);

        const betNeg = .{ .val = -beta.val, .dep = 255 - beta.dep };
        const alNeg = .{ .val = -alpha.val, .dep = 255 - alpha.dep };

        var val = try abSearch(b, betNeg, alNeg, if (dep == 1)
            1
        else
            dep - 1, undo.typ == null, alloc);

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

pub fn bestAb(b: *bo.Board, dep: u8, alloc: std.mem.Allocator) !tp.Move {
    var list = std.ArrayList(tp.Move).init(alloc);
    defer list.clearAndFree();
    _ = try mv.gen(b, &list);

    var move: ?tp.Move = null;
    var ret = SearchMin;

    for (list.items) |mov| {
        const undo = b.apply(mov);

        var val = try abSearch(b, SearchMin, SearchMax, dep - 1, false, alloc);
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
