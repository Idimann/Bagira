const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const se = @import("search.zig");
const nn = @import("nn.zig");

pub const Result = struct {
    pv: [se.MaxDepth]tp.Move,
    pv_size: u8,
    score: i32,
    depth: i12,
};

pub const Thread = struct {
    thread: ?std.Thread,

    board: bo.Board,
    nnw: nn.NN,
    search: se.Searcher,

    iter: f32,
    stopped: bool,
    res: Result,

    inline fn val(self: *const Thread, worst: i32) i32 {
        const max = std.math.maxInt(i16);
        const clamp1 = std.math.clamp(self.res.score - worst + 1, 0, max);
        const clamp2 = std.math.clamp(@as(i32, @intCast(self.res.depth)), 0, max);

        return clamp1 * clamp2;
    }
};

const PoolSize = 6;
var Pool: [PoolSize]Thread = undefined;
fn initPool(b: *const bo.Board, nnw: *nn.NN) !void {
    const float_size: f32 = @floatFromInt(PoolSize);
    const max_iter_add = std.math.clamp(float_size * 0.2, 1, 2) - 1;

    nnw.inputAccum(b);
    inline for (0..PoolSize) |i| {
        Pool[i].thread = null;

        Pool[i].board = b.*;
        Pool[i].nnw = nnw.*;

        Pool[i].search = try se.Searcher.init(&Pool[i], std.heap.c_allocator);

        Pool[i].iter = 1 + max_iter_add * (@as(f32, @floatFromInt(i + 1)) / float_size - 1);
        Pool[i].stopped = false;
        Pool[i].res = std.mem.zeroes(Result);
    }
}

inline fn deinitPool() void {
    inline for (0..PoolSize) |i| {
        Pool[i].thread.?.join();
        Pool[i].search.deinit();
    }
}

inline fn startSearch() !void {
    inline for (0..PoolSize) |i| {
        Pool[i].thread = try std.Thread.spawn(
            .{},
            se.Searcher.iterDeepening,
            .{&Pool[i].search},
        );
    }
}

inline fn stopSearch() void {
    for (0..PoolSize) |i| {
        Pool[i].stopped = true;
    }
}

pub fn bestMove(b: *bo.Board, nnw: *nn.NN, time: i64) !Result {
    const start = std.time.milliTimestamp();
    try initPool(b, nnw);

    try startSearch();

    // Time checking
    while (true) {
        if (std.time.milliTimestamp() - start >= time) break;
    }

    stopSearch();

    // Thread voting
    var worst_score: i32 = std.math.maxInt(i32);
    inline for (0..PoolSize) |i| {
        if (Pool[i].res.pv_size != 0) worst_score = @min(worst_score, Pool[i].res.score);
    }

    var votes = std.AutoHashMap(tp.Move, i32).init(std.heap.c_allocator);
    defer votes.deinit();

    inline for (0..PoolSize) |i| {
        const prev = votes.get(Pool[i].res.pv[0]) orelse 0;
        try votes.put(Pool[i].res.pv[0], prev + Pool[i].val(worst_score));
    }

    var best: *Thread = &Pool[0];
    inline for (1..PoolSize) |i| {
        const best_val = votes.get(best.res.pv[0]).?;
        const new_val = votes.get(Pool[i].res.pv[0]).?;
        const better = Pool[i].val(worst_score) * @intFromBool(Pool[i].res.pv_size > 2) >
            best.val(worst_score) * @intFromBool(best.res.pv_size > 2);

        // Update the best thread
        if (new_val > best_val or (new_val == best_val and better)) best = &Pool[i];
    }

    deinitPool();
    return best.res;
}
