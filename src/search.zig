const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const mv = @import("movegen.zig");
const ev = @import("eval.zig");

const MoveStage = enum {
    // Hash move
    TT,

    // Killer move
    Killer,

    // Captures
    GenCaptures,
    Captures,

    // Quiets
    GenQuiets,
    Quiets,

    // For quiet search
    GenQuiet,
    Quiet,
};

const MaxDepth = 255;
const History = struct {
    static: i32,
    stage: MoveStage,
    pv: ?tp.Move,
    killer: ?tp.Move,
};

pub const Searcher = struct {
    alloc: std.mem.Allocator,
    b: *bo.Board,
    stack: [2048]History,
    ply: u12,
    end_time: i64,

    history: [2][6][6][64]i32,

    pub const MateVal = std.math.maxInt(i32);
    const PieceValue = [_]i32{
        ev.PawnBase,
        ev.KnightBase,
        ev.BishopBase,
        ev.RookBase,
        ev.QueenBase,
    };

    pub fn init(b: *bo.Board, alloc: std.mem.Allocator, time: i64) Searcher {
        return .{
            .alloc = alloc,
            .b = b,
            .stack = std.mem.zeroes([2048]History),
            .ply = 0,
            .end_time = std.time.milliTimestamp() + time,
            .history = std.mem.zeroes([2][6][6][64]i32),
        };
    }

    inline fn historyDraw(self: *const Searcher) bool {
        // 50 move rule
        if (self.b.move_rule >= 100) return true;

        // Repetition
        if (self.b.hash_in > 1) {
            var iter: u8 = 2;

            while (iter <= self.b.move_rule) {
                if (self.b.hash[self.b.hash_in - iter] == self.b.hash[self.b.hash_in])
                    return true;

                iter += 2;
            }
        }

        return false;
    }

    // This creates some small variance to avoid 3 fold blindness
    inline fn drawVal() i32 {
        return @as(i3, undefined);
    }

    inline fn mateVal(ply: u12) i32 {
        return -MateVal + @as(i32, ply);
    }

    pub inline fn isMate(val: i32) bool {
        const mate = mateVal(std.math.maxInt(u12));
        return val <= mate or val >= -mate;
    }

    inline fn sort_moves(list: *std.ArrayList(tp.Move), score_list: *std.ArrayList(i32)) void {
        if (list.items.len < 2) return;

        for (0..(list.items.len / 2 + 1)) |i| {
            for (0..(list.items.len - i - 1)) |j| {
                if (score_list.items[j] < score_list.items[j + 1]) {
                    const temp = list.items[j];
                    const score_temp = score_list.items[j];
                    list.items[j] = list.items[j + 1];
                    score_list.items[j] = score_list.items[j + 1];
                    list.items[j + 1] = temp;
                    score_list.items[j + 1] = score_temp;
                }
            }
        }

        if (score_list.items[0] < score_list.items[1]) { //I know this is dumb
            const temp = list.items[0];
            const score_temp = score_list.items[0];
            list.items[0] = list.items[1];
            score_list.items[0] = score_list.items[0 + 1];
            list.items[1] = temp;
            score_list.items[1] = score_temp;
        }
    }

    fn nextMove(
        self: *Searcher,
        gen: *const mv.Maker,
        list: *std.ArrayList(tp.Move),
        score_list: *std.ArrayList(i32),
    ) !?tp.Move {
        switch (self.stack[self.ply].stage) {
            .TT => {
                self.stack[self.ply].stage = .Killer;
            },
            .Killer => {
                self.stack[self.ply].stage = .GenCaptures;
                if (self.ply > 0) {
                    if (self.stack[self.ply - 1].killer) |move| {
                        if (gen.isLegal(move)) return move;
                    }
                }
            },
            .GenCaptures => {
                try gen.gen(list, .Capture);
                score_list.items.len = 0;
                try score_list.ensureTotalCapacity(list.items.len);

                const block = &self.history[@intFromEnum(gen.b.side)];
                for (0..list.items.len) |i| {
                    const index1 = gen.b.pieceType(list.items[i].from);
                    const index2 = gen.b.pieceType(list.items[i].to);
                    const index3 = @intFromEnum(list.items[i].to);

                    const score = block.*[@intFromEnum(index1)][@intFromEnum(index2)][index3];
                    score_list.appendAssumeCapacity(score);
                }
                sort_moves(list, score_list);

                self.stack[self.ply].stage = .Captures;
            },
            .Captures => {
                if (list.items.len == 0)
                    self.stack[self.ply].stage = .GenQuiets
                else
                    return list.pop();
            },
            .GenQuiets => {
                try gen.gen(list, .Quiet);
                try gen.gen(list, .Castle);
                score_list.items.len = 0;
                try score_list.ensureTotalCapacity(list.items.len);

                const block = &self.history[@intFromEnum(gen.b.side)];
                for (0..list.items.len) |i| {
                    const index1 = gen.b.pieceType(list.items[i].from);
                    const index2 = 5;
                    const index3 = @intFromEnum(list.items[i].to);

                    const score = block.*[@intFromEnum(index1)][index2][index3];
                    score_list.appendAssumeCapacity(score);
                }
                sort_moves(list, score_list);

                self.stack[self.ply].stage = .Quiets;
            },
            .Quiets => {
                return list.popOrNull();
            },
            .GenQuiet => {
                if (gen.checks > 0)
                    try gen.gen(list, .Either)
                else
                    try gen.gen(list, .Capture);
                score_list.items.len = 0;
                try score_list.ensureTotalCapacity(list.items.len);

                const block = &self.history[@intFromEnum(gen.b.side)];
                for (0..list.items.len) |i| {
                    const index1 = gen.b.pieceType(list.items[i].from);
                    var index2: u6 = 5;
                    const index3 = @intFromEnum(list.items[i].to);
                    if (gen.b.w_pieces.check(list.items[i].to) or
                        gen.b.b_pieces.check(list.items[i].to))
                    {
                        index2 = @intFromEnum(gen.b.pieceType(list.items[i].to));
                    }
                    const score = block.*[@intFromEnum(index1)][index2][index3];
                    score_list.appendAssumeCapacity(score);
                }
                sort_moves(list, score_list);

                self.stack[self.ply].stage = .Quiet;
            },
            .Quiet => {
                return list.popOrNull();
            },
        }

        return self.nextMove(gen, list, score_list);
    }

    pub fn quietSearch(self: *Searcher, a: i32, b: i32) !i32 {
        // Check for three fold repetition and 50 move rule
        if (self.historyDraw()) return drawVal();

        if (self.ply >= MaxDepth) return ev.eval(self.b);

        var alpha = a;
        var beta = b;

        // Mate distance pruning (These are the best possible vals at this ply)
        alpha = @max(alpha, mateVal(self.ply));
        beta = @min(beta, -mateVal(self.ply + 1));
        if (alpha >= beta) return alpha;

        const gen = mv.Maker.init(self.b);

        // const pv = beta != alpha + 1;
        const inCheck = gen.checks > 0;

        // TODO: TT probe should be here

        const eval = if (!inCheck) ev.eval(self.b) else -MateVal;
        self.stack[self.ply].static = eval;
        if (eval >= alpha) alpha = eval;
        if (alpha >= beta) return alpha;

        var list = std.ArrayList(tp.Move).init(self.alloc);
        defer list.deinit();
        var score_list = std.ArrayList(i32).init(self.alloc);
        defer score_list.deinit();

        const futility = eval + PieceValue[0];

        var bestMove: ?tp.Move = null;
        var bestScore = alpha;
        var foundMove = false;

        self.stack[self.ply].stage = .GenQuiet;
        while (try self.nextMove(&gen, &list, &score_list)) |move| {
            foundMove = true;

            // Pruning, only if not in check
            if (!inCheck) {
                // Futility pruning
                if (self.b.w_pieces.check(move.to) or self.b.b_pieces.check(move.to)) {
                    if (futility +
                        PieceValue[@intFromEnum(self.b.pieceType(move.to))] <= alpha)
                        continue;
                }
            }

            const undo = self.b.apply(move);
            self.ply += 1;

            errdefer self.ply -= 1;
            errdefer self.b.remove(move, undo);

            const score = -(try self.quietSearch(-beta, -alpha));

            self.ply -= 1;
            self.b.remove(move, undo);

            if (score > bestScore) {
                bestScore = score;

                if (score > alpha) {
                    alpha = score;
                    bestMove = move;

                    if (score >= beta) break;
                }
            }
        }

        // Check and stalemate
        if (!foundMove and inCheck) return mateVal(self.ply);

        //TODO: TT insert should be here

        return bestScore;
    }

    pub fn search(self: *Searcher, a: i32, b: i32, depth: u8) !i32 {
        if (self.stack[0].pv != null and
            std.time.milliTimestamp() >= self.end_time) return error.NoTime;

        if (depth == 0 or self.ply >= MaxDepth) return self.quietSearch(a, b);

        // Check for three fold repetition and 50 move rule
        if (self.historyDraw()) return drawVal();

        var alpha = a;
        var beta = b;

        // Mate distance pruning (These are the best possible vals at this ply)
        alpha = @max(alpha, mateVal(self.ply));
        beta = @min(beta, -mateVal(self.ply + 1));
        if (alpha >= beta) return alpha;

        const gen = mv.Maker.init(self.b);

        const pv = beta != alpha + 1;
        const inCheck = gen.checks > 0;
        // const root = self.ply == 0;

        // TODO: TT probe should be here

        const eval = if (!inCheck) ev.eval(self.b) else -MateVal;
        self.stack[self.ply].static = eval;
        // const improving = if (self.ply > 1) eval - self.stack[self.ply - 2] > 0 else false;

        var list = std.ArrayList(tp.Move).init(self.alloc);
        defer list.deinit();
        var score_list = std.ArrayList(i32).init(self.alloc);
        defer score_list.deinit();

        var bestMove: ?tp.Move = null;
        var bestScore: i32 = -MateVal;

        var histories = std.mem.zeroes([64]?*i32);
        var move_counter: u6 = 0;

        self.stack[self.ply].stage = .TT;
        while (try self.nextMove(&gen, &list, &score_list)) |move| {
            const undo = self.b.apply(move);
            self.ply += 1;

            errdefer self.ply -= 1;
            errdefer self.b.remove(move, undo);

            var score: i32 = -(try self.search(-alpha - 1, -alpha, depth - 1));
            if (pv and score > alpha)
                score = -(try self.search(-beta, -alpha, depth - 1));

            self.ply -= 1;
            self.b.remove(move, undo);

            // Updating history
            const index1 = @intFromEnum(gen.b.pieceType(move.from));
            const index3 = @intFromEnum(move.to);
            if (self.b.side == .White) {
                const index2 = if (self.b.b_pieces.check(move.to))
                    5
                else
                    @intFromEnum(gen.b.pieceType(move.to));
                histories[move_counter] = &self.history[0][index1][index2][index3];
            } else {
                const index2 = if (self.b.w_pieces.check(move.to))
                    5
                else
                    @intFromEnum(gen.b.pieceType(move.to));
                histories[move_counter] = &self.history[1][index1][index2][index3];
            }
            move_counter += 1;

            if (score > bestScore) {
                bestScore = score;

                if (score > alpha) {
                    alpha = score;
                    bestMove = move;

                    if (score >= beta) {
                        // Updating history
                        histories[move_counter - 1].?.* += depth * depth;
                        for (0..(move_counter - 1)) |i|
                            histories[i].?.* -= (depth * depth) >> 1;

                        if (self.ply > 0) self.stack[self.ply - 1].killer = move;
                        break;
                    }
                }
            }
        }

        if (pv) {
            if (bestMove) |move| {
                self.stack[self.ply].pv = move;
            }
        }

        // Removing killer move
        self.stack[self.ply].killer = null;

        // Check and stalemate
        if (move_counter == 0) return if (inCheck) mateVal(self.ply) else drawVal();

        //TODO: TT insert should be here

        return bestScore;
    }

    pub fn aspiration(self: *Searcher, prev: i32, depth: u8) !i32 {
        const delta = 10 + @as(i32, @intCast(@divTrunc(@abs(prev), @as(u32, ev.PawnBase))));
        var alpha = prev - delta;
        var beta = prev + delta;

        while (true) {
            const score = try self.search(alpha, beta, depth);

            if (score <= alpha) {
                alpha -= delta;
                beta -= @divTrunc(delta, 4);
            } else if (score >= beta) {
                beta += delta;
                alpha += @divTrunc(delta, 4);
            } else {
                return score;
            }
        }
    }
};
