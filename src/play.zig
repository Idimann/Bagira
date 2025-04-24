const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const mv = @import("movegen.zig");

pub fn perft(b: *bo.Board, dep: usize, alloc: std.mem.Allocator) !usize {
    if (dep == 0) {
        return 1;
    }

    var list = std.ArrayList(tp.Move).init(alloc);
    _ = try mv.gen(b, &list);

    var ret: usize = 0;
    for (list.items) |mov| {
        const undo = b.apply(mov);
        const res = try perft(b, dep - 1, alloc);
        ret += res;
        try b.remove(mov, undo);
    }

    list.deinit();
    return ret;
}

pub fn perft_print(b: *bo.Board, dep: usize, alloc: std.mem.Allocator) !void {
    var list = std.ArrayList(tp.Move).init(alloc);
    _ = try mv.gen(b, &list);

    var total: usize = 0;
    for (list.items) |mov| {
        const undo = b.apply(mov);
        const res = try perft(b, dep - 1, alloc);
        mov.print();
        std.debug.print(": {}\n", .{res});
        try b.remove(mov, undo);

        total += res;
    }
    std.debug.print("Total: {}\n", .{total});

    list.deinit();
}
