const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const mv = @import("movegen.zig");
const po = @import("pool.zig");
const se = @import("search.zig");

pub fn perft(b: *bo.Board, dep: usize, alloc: std.mem.Allocator) !usize {
    if (dep == 0) {
        return 1;
    }

    var list = std.ArrayList(tp.Move).init(alloc);

    const maker = mv.Maker.init(b);
    try maker.gen(&list, .Either);
    try maker.gen(&list, .Castle);

    var ret: usize = 0;
    for (list.items) |mov| {
        const undo = b.apply(mov);
        const res = try perft(b, dep - 1, alloc);
        ret += res;
        b.remove(mov, undo);
    }

    list.deinit();
    return ret;
}

pub fn perft_print(b: *bo.Board, dep: usize, alloc: std.mem.Allocator) !void {
    var list = std.ArrayList(tp.Move).init(alloc);

    const maker = mv.Maker.init(b);
    try maker.gen(&list, .Either);
    try maker.gen(&list, .Castle);

    var total: usize = 0;
    for (list.items) |mov| {
        const undo = b.apply(mov);
        const res = try perft(b, dep - 1, alloc);
        mov.print();
        std.debug.print(": {}\n", .{res});
        b.remove(mov, undo);

        total += res;
    }
    std.debug.print("Total: {}\n", .{total});

    list.deinit();
}

pub fn play(b: *bo.Board, player: bo.Side, time: i64, minimal: bool) !void {
    if (b.side == player) {
        const stdin = std.io.getStdIn().reader();
        _ = try stdin.readByte();

        var buffer: [256]u8 = undefined;
        var fba = std.heap.FixedBufferAllocator.init(&buffer);
        const alloc = fba.allocator();
        var list = std.ArrayList(tp.Move).init(alloc);

        var gen = mv.Maker.init(b);
        try gen.gen(&list, .Either);
        try gen.gen(&list, .Castle);

        if (list.items.len == 0) {
            if (gen.checks > 0)
                std.debug.print("You lost!\n", .{})
            else
                std.debug.print("It's a draw!\n", .{});
            return;
        }

        mv.printList(&list);
        var buf: [3]u8 = undefined;
        while (try stdin.readUntilDelimiterOrEof(buf[0..], '\n')) |user_input| {
            const in = try std.fmt.parseInt(u8, user_input, 10);
            if (list.items.len < in) continue;

            _ = b.apply(list.items[in - 1]);
            std.debug.print(" You played: ", .{});
            list.items[in - 1].print();
            std.debug.print("\n", .{});
            break;
        }
    } else {
        const best = try po.bestMove(b, time);
        if (best.move) |move| {
            _ = b.apply(move);
            if (!minimal) b.print();
            move.print();
            std.debug.print("\n", .{});
            std.debug.print(" => Score: {}, Bound: {}\n", .{ best.bp.p, best.bp.bound });
        } else {
            const gen = mv.Maker.init(b);
            if (gen.checks > 0)
                std.debug.print("You won!\n", .{})
            else
                std.debug.print("It's a draw!\n", .{});
            return;
        }
    }

    try play(b, player, time, minimal);
}

pub fn selfPlay(b: *bo.Board, time1: i64, time2: i64, minimal: bool) !void {
        const best = try po.bestMove(b, if (b.side == .White) time1 else time2);
        if (best.move) |move| {
            _ = b.apply(move);
            if (!minimal) b.print();
            move.print();
            std.debug.print("\n", .{});
            std.debug.print(" => Score: {}, Bound: {}\n", .{ best.bp.p, best.bp.bound });
    } else {
        const gen = mv.Maker.init(b);
        if (gen.checks > 0) {
            const side: bo.Side = @enumFromInt(~@intFromEnum(b.side));
            if (side == .White)
                std.debug.print("White won!\n", .{})
            else
                std.debug.print("Black won!\n", .{});
        } else std.debug.print("It's a draw!\n", .{});
        return;
    }

    try selfPlay(b, time1, time2, minimal);
}
