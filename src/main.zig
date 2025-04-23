const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const pos = @import("positions.zig");
const tab = @import("tablegen.zig");
const pl = @import("play.zig");

pub fn main() !void {
    tab.initLines();
    tab.initDiags();

    var b = try bo.Board.fromFen(pos.start);

    var buf: [512 * @sizeOf(tp.Move)]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buf);
    const alloc = fba.allocator();

    try pl.perft_print(&b, 3, alloc);
}
