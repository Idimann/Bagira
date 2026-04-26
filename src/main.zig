const std = @import("std");
const bo = @import("board.zig");
const pos = @import("positions.zig");
const tab = @import("tablegen.zig");
const pl = @import("play.zig");
const nn = @import("nn.zig");
const zbr = @import("zobrist.zig");
const po = @import("pool.zig");
const tt = @import("tt.zig");

pub fn main() !void {
    tab.initLines();
    tab.initDiags();
    try zbr.init();
    try tt.init(128, std.heap.c_allocator);
    defer tt.deinit(std.heap.c_allocator);

    var nnw = try nn.NN.init("networks/", "v2_2.bin");
    var b = try bo.Board.fromFen("8/8/8/4Q3/2k1p3/4K3/2P5/8 w -- 7 51");

    try pl.play(&b, &nnw, .Black, 1000, false);
    // try pl.selfPlay(&b, &nnw, 600, true);
}
