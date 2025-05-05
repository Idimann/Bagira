const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const se = @import("search.zig");

pub const Result = struct {
    move: tp.Move,
    eval: i32,
    dep: u8,
};

pub fn bestMove(b: *bo.Board, time: i64, minimal: bool) !?Result {
    const alloc = std.heap.c_allocator;
    var search = se.Searcher.init(b, alloc, time);

    var dep: u8 = 1; // This seems more efficient than starting at 1 for some reason
    var prev: i32 = 0;
    var stack = search.stack;
    while (true) {
        prev = (if (dep == 1)
            search.search(-se.Searcher.MateVal, se.Searcher.MateVal, dep)
        else
            search.aspiration(prev, dep)) catch |err| {
            switch (err) {
                error.NoTime => break,
                else => return err,
            }
        };
        stack = search.stack;

        if (!minimal) {
            std.debug.print("PV at {}:\n", .{dep});
            for (0..2048) |i| {
                if (stack[i].pv) |move| {
                    std.debug.print("\t=> ", .{});
                    move.print();
                    std.debug.print("\n", .{});
                } else break;
            }
        }

        dep += 1;
        if (stack[0].pv == null) break; // We can't move (stalemate or mate)
        if (se.Searcher.isMate(prev) and prev > 0) break;
    }

    if (stack[0].pv) |move| {
        return .{ .move = move, .eval = prev, .dep = dep - 1 };
    }

    return null;
}
