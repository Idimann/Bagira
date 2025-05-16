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

    stopped: bool,
    res: ?Result,
};

const PoolSize = 8;
var Pool: [PoolSize]Thread = undefined;
fn initPool(b: *const bo.Board) !void {
    for (0..PoolSize) |i| {
        Pool[i].thread = null;

        Pool[i].board = b.*;
        Pool[i].search = try se.Searcher.init(&Pool[i], std.heap.c_allocator);

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
    const sleep_time = 50;
    for (0..PoolSize) |i| {
        Pool[i].thread = try std.Thread.spawn(
            .{},
            se.Searcher.iterDeepening,
            .{&Pool[i].search},
        );
        std.time.sleep(sleep_time); // Sleep some time for larger diff between the threads
    }
}

fn stop_search() void {
    for (0..PoolSize) |i| {
        Pool[i].stopped = true;
    }
}

pub fn bestMove(b: *const bo.Board, time: u64) !?Result {
    try initPool(b);

    try start_search();
    std.time.sleep(std.time.ns_per_ms * time); // Time checking
    stop_search();

    // TODO: Thread voting should be here
    const ret = Pool[0].res;

    deinitPool();
    return ret;
}
