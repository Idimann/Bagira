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

pub fn main() !void {
    tab.initLines();
    tab.initDiags();
    try zbr.init();

    var b = try bo.Board.fromFen(pos.start);
    std.debug.print("Static: {}\n", .{ev.eval(&b)});

    try pl.play(&b, .White, 2000, false);
}
