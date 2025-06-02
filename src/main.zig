const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const pos = @import("positions.zig");
const tab = @import("tablegen.zig");
const pl = @import("play.zig");
const ev = @import("eval.zig");
const mv = @import("movegen.zig");
const zbr = @import("zobrist.zig");
const po = @import("pool.zig");
const se = @import("search.zig");
const tt = @import("tt.zig");
const da = @import("datagen.zig");

pub fn main() !void {
    tab.initLines();
    tab.initDiags();
    try zbr.init();

    var nn = try ev.NN.init();
    for (0..25) |i| {
        var b = try bo.Board.fromFen(pos.start);

        try zbr.init();
        tt.clear();
        try da.play(&b, &nn, 250);
        std.debug.print("Finished {}!\n", .{i + 1});
    }

    // try pl.play(&b, &nn, .White, 1000, false);
}
