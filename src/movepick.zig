const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const se = @import("search.zig");

pub const Result = struct {
    move: tp.Move,
    eval: i32,
    dep: i12,
};

pub fn bestMove(b: *bo.Board, time: i64, minimal: bool) !?Result {
    const alloc = std.heap.c_allocator;
    var search = try se.Searcher.init(b, alloc, time);

    const iter = 1;
    var dep: i12 = iter;
    var prev: i32 = 0;
    var pv = search.stack[0].pv;
    var pv_size = search.stack[0].pv_size;
    while (true) {
        prev = (if (dep == 1)
            search.search(-se.Searcher.MateVal, se.Searcher.MateVal, dep, false)
        else
            search.aspiration(prev, dep)) catch |err| {
            switch (err) {
                error.NoTime => break,
                else => return err,
            }
        };
        pv = search.stack[0].pv;
        pv_size = search.stack[0].pv_size;
        search.clearStack();

        if (!minimal) {
            std.debug.print("PV at {}:\n", .{dep});
            for (0..pv_size) |i| {
                std.debug.print("\t=> ", .{});
                pv[i].print();
                std.debug.print("\n", .{});
            }
        }

        dep += iter;
        if (pv_size == 0) break; // We can't move (stalemate or mate)
        if (se.Searcher.isMate(prev)) break;
    }

    search.deinit();
    if (pv_size != 0) {
        return .{ .move = pv[0], .eval = prev, .dep = dep - iter };
    }

    return null;
}
