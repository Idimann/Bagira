const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const pos = @import("positions.zig");
const tab = @import("tablegen.zig");
const pl = @import("play.zig");
const se = @import("search.zig");
const ev = @import("eval.zig");

pub fn main() !void {
    tab.initLines();
    tab.initDiags();
    try se.initRandom();

    var his_buf: [350 * @sizeOf(u64)]u8 = undefined;
    var his_fba = std.heap.FixedBufferAllocator.init(&his_buf);
    const his_alloc = his_fba.allocator();
    var b = try bo.Board.fromFen(pos.Openings.caro_kann, his_alloc);
    b.print();

    std.debug.print("{}\n", .{ev.eval(&b)});

    // const depth = 500;
    // const size = 150;
    // // This is a pretty good formular, assuming a maximum of 128 moves in any position
    // var buf: [
    //     depth * (@sizeOf(tp.Move) + @sizeOf(tp.Remove)) +
    //         size * @sizeOf(tp.Move)
    // ]u8 = undefined;
    // var fba = std.heap.FixedBufferAllocator.init(&buf);
    // const alloc = fba.allocator();
    //
    // var eval: f16 = 0;
    // var count: f16 = 0;
    // for (0..100) |_| {
    //     eval += (try se.carloSearch(&b, alloc)).val;
    //     count += 1;
    // }
    // std.debug.print("{}\n", .{eval / count});
}
