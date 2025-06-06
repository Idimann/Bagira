const std = @import("std");
const bo = @import("board.zig");
const pos = @import("positions.zig");
const tab = @import("tablegen.zig");
const pl = @import("play.zig");
const nn = @import("nn.zig");
const zbr = @import("zobrist.zig");
const po = @import("pool.zig");

pub fn main() !void {
    tab.initLines();
    tab.initDiags();
    try zbr.init();

    var nnw = try nn.NN.init("networks/", "v2_0.bin");
    var b = try bo.Board.fromFen(pos.start);

    // std.debug.print("{}\n", .{try pl.legals(&b, 1, std.heap.c_allocator)});
    // try pl.legals_print(&b, 4, std.heap.c_allocator);
    try pl.play(&b, &nnw, .Black, 1000, false);
}
