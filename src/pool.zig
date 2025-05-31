const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const se = @import("search.zig");

pub const Result = struct {
    bp: se.Searcher.BoundProb,
    move: ?tp.Move,
};

pub const Thread = struct {
    thread: ?std.Thread,

    board: bo.Board,
    search: se.Searcher,

    iter: se.Searcher.Prob,
    stopped: bool,
    res: Result,

    inline fn val(self: *const Thread) se.Searcher.Prob {
        return self.res.bp.p * (1 - self.res.bp.bound);
    }
};

const PoolSize = 6;
var Pool: [PoolSize]Thread = undefined;
fn initPool(b: *const bo.Board) !void {
    const float_size: f32 = @floatFromInt(PoolSize);
    const max_iter_add = std.math.clamp(1 - float_size * 0.02, 0.75, 1);

    inline for (0..PoolSize) |i| {
        Pool[i].thread = null;

        Pool[i].board = b.*;
        Pool[i].search = se.Searcher.init(&Pool[i], std.heap.c_allocator);

        Pool[i].iter = 1 - (1 - max_iter_add) *
            @as(se.Searcher.Prob, @floatFromInt(i + 1)) / float_size;
        Pool[i].stopped = false;
        Pool[i].res = std.mem.zeroes(Result);
    }
}

inline fn deinitPool() void {
    inline for (0..PoolSize) |i| {
        Pool[i].thread.?.join();
        // Pool[i].search.deinit();
    }
}

inline fn startSearch() !void {
    inline for (0..PoolSize) |i| {
        Pool[i].thread = try std.Thread.spawn(
            .{},
            se.Searcher.deepening,
            .{&Pool[i].search},
        );
    }
}

inline fn stopSearch() void {
    for (0..PoolSize) |i| {
        Pool[i].stopped = true;
    }
}

pub fn bestMove(b: *bo.Board, time: i64) !Result {
    const start = std.time.milliTimestamp();
    try initPool(b);

    try startSearch();

    // Time checking
    while (true) {
        if (std.time.milliTimestamp() - start >= time) break;
    }

    stopSearch();

    // Thread voting
    var votes = std.AutoHashMap(tp.Move, se.Searcher.Prob).init(std.heap.c_allocator);
    defer votes.deinit();

    inline for (0..PoolSize) |i| {
        const prev = votes.get(Pool[i].res.move.?) orelse 0;
        try votes.put(Pool[i].res.move.?, prev + Pool[i].val());
    }

    var best: *Thread = &Pool[0];
    inline for (1..PoolSize) |i| {
        const best_val = votes.get(best.res.move.?).?;
        const new_val = votes.get(Pool[i].res.move.?).?;
        const better = Pool[i].val() > best.val();

        if (new_val > best_val or (new_val == best_val and better)) best = &Pool[i];
    }

    deinitPool();
    return best.res;
}
