const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const pos = @import("positions.zig");
const tab = @import("tablegen.zig");
const pl = @import("play.zig");
const en = @import("engine.zig");
const ev = @import("eval.zig");

pub fn main() !void {
    tab.initLines();
    tab.initDiags();

    var b = try bo.Board.fromFen(pos.Middlegames.spanish);
    b.print();

    std.debug.print("{}\n", .{ev.eval(&b)});

    // const size = 0x1000;
    // // This is a pretty good formular, assuming a maximum of 128 moves in any position
    // var buf: [size * @sizeOf(tp.Move)]u8 = undefined;
    // var fba = std.heap.FixedBufferAllocator.init(&buf);
    // const alloc = fba.allocator();
    //
    // const best = try en.bestAb(&b, 5, alloc);
    // best.print();
    // std.debug.print("\n", .{});
}
