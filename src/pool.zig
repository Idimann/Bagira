const std = @import("std");
const tp = @import("types.zig");
const mv = @import("movegen.zig");
const bo = @import("board.zig");
const se = @import("search.zig");
const nn = @import("nn.zig");

pub const RootMove = struct {
    move: tp.Move,
    pv: [se.MaxDepth]tp.Move,
    pv_size: u8,
    depth: i12,
    score: i32,
    avg_score: i32,
    avg_score_sq: i32,
    nodes: usize,
};

pub const Thread = struct {
    thread: ?std.Thread,

    board: bo.Board,
    nnw: nn.NN,
    search: se.Searcher,

    iter: f32,
    stopped: bool,
    root_moves: std.ArrayList(RootMove),
    best_root: RootMove,
    nodes: usize,

    inline fn val(self: *const Thread, worst: i32) i32 {
        const max = std.math.maxInt(i16);
        const clamp1 = std.math.clamp(self.best_root.score - worst + 1, 0, max);
        const clamp2 = std.math.clamp(
            @as(i32, @intCast(self.best_root.depth)),
            0,
            max,
        );

        return clamp1 * clamp2;
    }

    pub inline fn sortRootMoves(self: *Thread) void {
        if (self.root_moves.items.len < 2) return;

        for (0..(self.root_moves.items.len - 1)) |i| {
            for (0..(self.root_moves.items.len - i - 1)) |j| {
                if (self.root_moves.items[j].score < self.root_moves.items[j + 1].score) {
                    const temp = self.root_moves.items[j];
                    self.root_moves.items[j] = self.root_moves.items[j + 1];
                    self.root_moves.items[j + 1] = temp;
                }
            }
        }

        self.best_root = self.root_moves.items[0];
    }
};

const PoolSize = 8;
var Pool: [PoolSize]Thread = undefined;
fn initPool(b: *const bo.Board, nnw: *nn.NN) !bool {
    const float_size: f32 = @floatFromInt(PoolSize);
    const max_iter_add = std.math.clamp(float_size * 0.2, 1, 1.5) - 1;

    nnw.inputAccum(b);

    var list = std.ArrayList(tp.Move).init(std.heap.c_allocator);
    defer list.deinit();
    const gen = mv.Maker.init(b);
    try gen.gen(&list, .Either);
    try gen.gen(&list, .Castle);

    if (list.items.len == 0) return true;

    inline for (0..PoolSize) |i| {
        Pool[i].thread = null;

        Pool[i].board = b.*;
        Pool[i].nnw = nnw.*;

        Pool[i].search = try se.Searcher.init(&Pool[i], std.heap.c_allocator);

        Pool[i].iter = 1 + max_iter_add * (@as(f32, @floatFromInt(i + 1)) / float_size);
        Pool[i].stopped = false;

        Pool[i].root_moves = std.ArrayList(RootMove).init(std.heap.c_allocator);

        try Pool[i].root_moves.ensureTotalCapacity(list.items.len);
        for (list.items) |move| {
            Pool[i].root_moves.appendAssumeCapacity(.{
                .move = move,
                .pv = std.mem.zeroes([se.MaxDepth]tp.Move),
                .pv_size = 0,
                .depth = 0,
                .score = -se.Searcher.MateVal,
                .avg_score = 0,
                .avg_score_sq = 0,
                .nodes = 0,
            });
        }

        Pool[i].best_root = Pool[i].root_moves.items[0];
        Pool[i].nodes = 0;
    }

    return false;
}

inline fn deinitPool() void {
    inline for (0..PoolSize) |i| {
        Pool[i].thread.?.join();
        Pool[i].search.deinit();
        Pool[i].root_moves.deinit();
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

pub fn bestMove(b: *bo.Board, nnw: *nn.NN, time: i64) !?RootMove {
    const start = std.time.milliTimestamp();
    if (try initPool(b, nnw)) return null;

    try startSearch();

    // Time checking
    while (true) {
        if (std.time.milliTimestamp() - start >= time) break;
    }

    stopSearch();

    // Thread voting
    var worst_score: i32 = std.math.maxInt(i32);
    inline for (0..PoolSize) |i| {
        worst_score = @min(worst_score, Pool[i].best_root.score);
    }

    var votes = std.AutoHashMap(tp.Move, i32).init(std.heap.c_allocator);
    defer votes.deinit();

    inline for (0..PoolSize) |i| {
        const prev = votes.get(Pool[i].best_root.move) orelse 0;
        try votes.put(Pool[i].best_root.move, prev + Pool[i].val(worst_score));
    }

    var best: *Thread = &Pool[0];
    inline for (1..PoolSize) |i| {
        const best_val = votes.get(best.best_root.move).?;
        const new_val = votes.get(Pool[i].best_root.move).?;
        const better = Pool[i].val(worst_score) *
            @intFromBool(Pool[i].best_root.pv_size > 2) >
            best.val(worst_score) * @intFromBool(best.best_root.pv_size > 2);

        // Update the best thread
        if (new_val > best_val or (new_val == best_val and better)) best = &Pool[i];
    }

    const ret = best.best_root;
    deinitPool();
    return ret;
}
