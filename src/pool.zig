const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const se = @import("search.zig");

pub const Result = struct {
    move: tp.Move,
    score: i32,
    depth: i12,
};

pub const Thread = struct {
    thread: ?std.Thread,

    board: bo.Board,
    search: se.Searcher,

    iter: f32,
    stopped: bool,
    res: ?Result,
};

const PoolSize = 32;
var Pool: [PoolSize]Thread = undefined;
fn initPool(b: *const bo.Board) !void {
    const float_size: f32 = @floatFromInt(PoolSize);
    const max_iter = @min(2.5, float_size * 0.1);

    for (0..PoolSize) |i| {
        Pool[i].thread = null;

        Pool[i].board = b.*;
        Pool[i].search = try se.Searcher.init(&Pool[i], std.heap.c_allocator);

        Pool[i].iter = 1 + (max_iter - 1) * (@as(f32, @floatFromInt(i)) / (float_size - 1));
        Pool[i].stopped = false;
        Pool[i].res = null;
    }
}

fn deinitPool() void {
    for (0..PoolSize) |i| {
        Pool[i].thread.?.join();
        Pool[i].search.deinit();
    }
}

fn start_search() !void {
    for (0..PoolSize) |i| {
        Pool[i].thread = try std.Thread.spawn(
            .{},
            se.Searcher.iterDeepening,
            .{&Pool[i].search},
        );
    }
}

fn stop_search() void {
    for (0..PoolSize) |i| {
        Pool[i].stopped = true;
    }
}

pub fn bestMove(b: *bo.Board, time: i64) !?Result {
    const start = std.time.milliTimestamp();
    try initPool(b);

    try start_search();

    // Time checking
    while (true) {
        if (std.time.milliTimestamp() - start >= time) break;
    }

    stop_search();

    // TODO: Thread voting should be here
    const ret = Pool[0].res;

    deinitPool();
    return ret;
}
