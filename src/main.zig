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

    var nnw = try nn.NN.init("networks/", "first.bin");

    var b = try bo.Board.fromFen(pos.start);
    try pl.play(&b, &nnw, .White, 2000, false);
}
