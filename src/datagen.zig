const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const mv = @import("movegen.zig");
const po = @import("pool.zig");
const se = @import("search.zig");
const ev = @import("eval.zig");

const Folder = "training/data/";
const Name = "first";
inline fn write(list: *const std.ArrayList([]u8), append: *const [7:0]u8) !void {
    const file = try std.fs.cwd().openFile(Folder ++ Name, .{ .mode = .write_only });
    defer file.close();

    try file.seekFromEnd(0);
    for (list.items) |str| {
        try file.writeAll(str);
        try file.writeAll(append);
        std.heap.c_allocator.free(str);
    }
}

pub fn play(b: *bo.Board, nn: *ev.NN, time: i64) !void {
    var list = std.ArrayList([]u8).init(std.heap.c_allocator);
    defer list.deinit();

    while (true) {
        const best = try po.bestMove(b, nn, time);
        if (best.depth != 0) {
            const score = std.math.clamp(@divTrunc(if (b.side == .White)
                best.score
            else
                -best.score, ev.CentiPawn), -4000, 4000);

            var buf: [53]u8 = undefined;
            const score_len = std.fmt.formatIntBuf(&buf, score, 10, .lower, .{});

            const fen = try b.toFen(std.heap.c_allocator);
            defer fen.deinit();

            var insert = try std.heap.c_allocator.alloc(u8, fen.items.len + 3 + score_len);

            std.mem.copyForwards(u8, insert, fen.items);

            insert[fen.items.len] = ' ';
            insert[fen.items.len + 1] = '|';
            insert[fen.items.len + 2] = ' ';

            std.mem.copyForwards(u8, insert[fen.items.len + 3 ..], buf[0..score_len]);

            try list.append(insert);

            _ = b.apply(best.pv[0]);
        } else {
            const gen = mv.Maker.init(b);
            if (gen.checks > 0) {
                const side: bo.Side = @enumFromInt(~@intFromEnum(b.side));
                try if (side == .White)
                    write(&list, " | 1.0\n")
                else
                    write(&list, " | 0.0\n");
            } else try write(&list, " | 0.5\n");
            return;
        }
    }
}
