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

    var b = try bo.Board.fromFen(pos.Openings.caro_kann);
    b.print();

    std.debug.print("{}\n", .{ev.eval(&b)});
}
