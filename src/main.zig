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
const tt2 = @import("tt2.zig");
const se2 = @import("search2.zig");

pub fn main() !void {
    tab.initLines();
    tab.initDiags();
    try zbr.init();

    var b = try bo.Board.fromFen(pos.start);
    std.debug.print("Static: {}\n", .{ev.eval(&b)});

    var searcher = se2.Searcher.init(&b, std.heap.c_allocator);

    var bound: se2.Searcher.Prob = 1;
    var result: se2.Searcher.Prob = 0.0;
    while (bound >= 0.1) {
        const score = try searcher.search(bound, 1);
        result = score.p;
        bound = @min(score.bound, bound * 0.9);
    }
    std.debug.print("Ev {}\n", .{result});
    // try pl.selfPlay(&b, 8000, 8000, false);
}
