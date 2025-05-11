const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const mv = @import("movegen.zig");
const ev = @import("eval.zig");
const tt = @import("tt.zig");

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

fn initReductions() [2][32][32]i12 {
    var ret = std.mem.zeroes([2][32][32]i12);

    for (1..32) |i| {
        for (1..32) |j| {
            const log = @log(@as(f64, @floatFromInt(i))) * @log(@as(f64, @floatFromInt(j)));

            ret[0][i][j] = @intFromFloat(0.38 + log / 3.76);
            ret[1][i][j] = @intFromFloat(2.01 + log / 2.32);
        }
    }

    return ret;
}
const Reductions = initReductions();

const MaxDepth = 255;
const History = struct {
    static: i32,
    stage: MoveStage,
    pv: [MaxDepth]tp.Move,
    pv_size: u8,
    killer: ?tp.Move,
};

pub const Searcher = struct {
    alloc: std.mem.Allocator,
    b: *bo.Board,
    stack: []History,
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

    pub fn init(b: *bo.Board, alloc: std.mem.Allocator, time: i64) !Searcher {
        const stack = try alloc.alloc(History, MaxDepth);
        for (0..MaxDepth) |i| stack[i] = std.mem.zeroes(History);

        return .{
            .alloc = alloc,
            .b = b,
            .stack = stack,
            .ply = 0,
            .end_time = std.time.milliTimestamp() + time,
            .history = std.mem.zeroes([2][6][6][64]i32),
        };
    }

    pub fn deinit(self: *Searcher) void {
        self.alloc.free(self.stack);
    }

    pub inline fn clearStack(self: *Searcher) void {
        for (0..MaxDepth) |i| self.stack[i] = std.mem.zeroes(History);
    }

    pub inline fn timeDiff(self: *Searcher) i64 {
        return self.end_time - std.time.milliTimestamp();
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

    fn updatePv(self: *Searcher, move: tp.Move) void {
        const from = @min(MaxDepth - 1, self.stack[self.ply + 1].pv_size);

        for (0..from) |i| self.stack[self.ply].pv[i + 1] = self.stack[self.ply + 1].pv[i];
        self.stack[self.ply].pv_size = from + 1;
        self.stack[self.ply].pv[0] = move;
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
        tte: tt.TT_Result,
    ) !?tp.Move {
        switch (self.stack[self.ply].stage) {
            .TT => {
                self.stack[self.ply].stage = .Killer;
                if (tte.typ == .Fine and gen.isLegal(tte.entry.move))
                    return tte.entry.move;
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
                else {
                    _ = score_list.pop();
                    return list.pop();
                }
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
                _ = score_list.popOrNull();
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
                _ = score_list.popOrNull();
                return list.popOrNull();
            },
        }

        return self.nextMove(gen, list, score_list, tte);
    }

    pub fn quietSearch(self: *Searcher, a: i32, b: i32) !i32 {
        // Check for three fold repetition and 50 move rule
        if (self.historyDraw()) return drawVal();
        if (self.ply >= MaxDepth) return ev.eval(self.b);

        // Mate distance pruning (These are the best possible vals at this ply)
        var alpha = @max(a, mateVal(self.ply));
        const beta = @min(b, -mateVal(self.ply + 1));
        if (alpha >= beta) return alpha;

        const gen = mv.Maker.init(self.b);

        const pv = b != a + 1;
        const inCheck = gen.checks > 0;

        // TT Probe
        const tte = tt.probe(self.b);

        // Trust the tt entry if it's usable
        if (!pv and
            tte.typ == .Fine and
            tte.entry.usable(alpha, beta))
            return tte.entry.score;

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
        while (try self.nextMove(&gen, &list, &score_list, tte)) |move| {
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
                bestMove = move;

                if (score > alpha) {
                    alpha = score;

                    // if (pv) {
                    //     self.updatePv(move);
                    //     self.stack[self.ply + 1].pv_size = 0;
                    // }

                    if (score >= beta) break;
                }
            }
        }

        // Check and stalemate
        if (!foundMove and inCheck) return mateVal(self.ply);

        // TT insert
        tt.store(
            self.b,
            bestScore,
            0,
            alpha,
            beta,
            bestMove.?,
            @intCast((self.b.hash_in - self.ply) % std.math.maxInt(u6)),
            tte,
        );

        return bestScore;
    }

    pub fn search(self: *Searcher, a: i32, b: i32, depth: i12, cutnode: bool) !i32 {
        if (std.time.milliTimestamp() >= self.end_time) return error.NoTime;

        if (depth == 0 or self.ply >= MaxDepth) return self.quietSearch(a, b);

        // Check for three fold repetition and 50 move rule
        if (self.historyDraw()) return drawVal();

        // Mate distance pruning (These are the best possible vals at this ply)
        var alpha = @max(a, mateVal(self.ply));
        const beta = @min(b, -mateVal(self.ply + 1));
        if (alpha >= beta) return alpha;

        const gen = mv.Maker.init(self.b);

        const inCheck = gen.checks > 0;
        const pv = b != a + 1;
        const root = self.ply == 0;

        // TT Probe
        const tte = tt.probe(self.b);

        // Trust the tt entry if it's usable
        if (!pv and
            tte.typ == .Fine and
            tte.entry.depth >= depth and
            tte.entry.usable(alpha, beta))
            return tte.entry.score;

        const eval = if (tte.typ == .Fine) tte.entry.score else ev.eval(self.b);
        self.stack[self.ply].static = eval;
        const improving = !inCheck and
            (self.ply <= 1 or eval > self.stack[self.ply - 2].static);

        // Real pruning
        if (!inCheck and !improving and !root and !pv and !isMate(beta)) {
            const futility = eval + PieceValue[0];

            // Reverse futility pruning
            if (depth < 5 and futility >= beta and tte.typ != .Fine) return eval;
        }

        var list = std.ArrayList(tp.Move).init(self.alloc);
        defer list.deinit();
        var score_list = std.ArrayList(i32).init(self.alloc);
        defer score_list.deinit();

        // const futility = eval + PieceValue[0] + @divTrunc(PieceValue[0], 2);

        var bestMove: ?tp.Move = null;
        var bestScore: i32 = -MateVal;

        var histories = std.mem.zeroes([64]?*i32);
        var move_counter: u6 = 0;

        self.stack[self.ply].stage = .TT;
        while (try self.nextMove(&gen, &list, &score_list, tte)) |move| {
            const undo = self.b.apply(move);

            errdefer self.b.remove(move, undo);
            errdefer self.ply -= 1;

            var score: i32 = undefined;
            var next_depth = depth - 1;

            // Check extensions
            if (self.ply < 3 and inCheck) next_depth += 1;

            const quiet = undo.typ == null;

            // Pruning
            const min_count = @max(
                0,
                @as(u3, @intFromBool(pv)) +
                    @as(u3, @intFromBool(root)) +
                    @as(u3, @intFromBool(!quiet)),
            );
            if (depth > 2 and !root and move_counter > min_count) {
                var R: i12 = 0;

                // LMR
                R += Reductions[@intFromBool(quiet)][@intCast(depth)][@intCast(self.ply)];

                if (cutnode) R += 2;

                // Various pruning reduction criteria
                if (pv) R -= 1;
                if (inCheck) R -= 1;
                if (self.stack[self.ply].stage == .Killer) R -= 1;
                if (improving) R -= 1;

                const r_depth = std.math.clamp(
                    next_depth - R,
                    @min(1, next_depth),
                    next_depth,
                );

                self.ply += 1;
                score = -(try self.search(-alpha - 1, -alpha, r_depth, true));
                self.ply -= 1;

                if (score > alpha and r_depth < next_depth) {
                    self.ply += 1;
                    score = -(try self.search(-alpha - 1, -alpha, next_depth, !cutnode));
                    self.ply -= 1;
                }
            } else if (!pv or move_counter > 0) {
                self.ply += 1;
                score = -(try self.search(-alpha - 1, -alpha, next_depth, !cutnode));
                self.ply -= 1;
            }

            self.ply += 1;
            if (pv and (move_counter == 0 or score > alpha))
                score = -(try self.search(-beta, -alpha, next_depth, false));
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
                bestMove = move;

                if (score > alpha) {
                    alpha = score;
                    if (pv) {
                        self.updatePv(move);
                        self.stack[self.ply + 1].pv_size = 0;
                    }

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

        // Removing killer move
        self.stack[self.ply].killer = null;

        // Check and stalemate
        if (move_counter == 0) return if (inCheck) mateVal(self.ply) else drawVal();

        // TT insert
        tt.store(
            self.b,
            bestScore,
            depth,
            alpha,
            beta,
            bestMove.?,
            @intCast((self.b.hash_in - self.ply) % std.math.maxInt(u6)),
            tte,
        );

        return bestScore;
    }

    pub fn aspiration(self: *Searcher, prev: i32, depth: i12) !i32 {
        var delta = 10 + @as(i32, @intCast(@divTrunc(@abs(prev), @as(u32, ev.PawnBase))));
        var alpha = prev - delta;
        var beta = prev + delta;

        while (true) {
            const score = try self.search(alpha, beta, depth, false);
            if (isMate(score)) return score;

            if (score <= alpha) {
                beta = @divFloor(alpha + beta, 2) + 1;
                alpha -= delta;
            } else if (score >= beta) {
                beta += delta;
            } else return score;

            delta += @divFloor(delta, 3);
        }
    }
};
