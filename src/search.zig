const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const mv = @import("movegen.zig");
const ev = @import("eval.zig");
const tt = @import("tt.zig");
const po = @import("pool.zig");
const pi = @import("movepick.zig");
const hi = @import("history.zig");

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

pub const MaxDepth = 255;
const History = struct {
    static: i32,
    stage: ?pi.Stage,
    pv: [MaxDepth]tp.Move,
    pv_size: u8,
    killer: ?tp.Move,
    move: ?tp.Move,
    hist_score: ?i32,
};

pub const Searcher = struct {
    alloc: std.mem.Allocator,
    b: *bo.Board,
    nn: *ev.NN,
    stack: []History,
    start_ply: u12,
    pv_exists: bool,
    thread: *po.Thread,

    stats: hi.Stats,

    // The division by 2 is for overflow protection
    pub const MateVal = std.math.maxInt(i32) >> 1;
    const PieceValue = [_]i32{
        ev.PawnBase,
        ev.KnightBase,
        ev.BishopBase,
        ev.RookBase,
        ev.QueenBase,
    };

    pub fn init(thread: *po.Thread, alloc: std.mem.Allocator) !Searcher {
        const stack = try alloc.alloc(History, MaxDepth);
        inline for (0..MaxDepth) |i| stack[i] = std.mem.zeroes(History);

        return .{
            .alloc = alloc,
            .b = &thread.board,
            .nn = &thread.nn,
            .stack = stack,
            .start_ply = thread.board.hash_in,
            .pv_exists = false,
            .thread = thread,
            .stats = hi.Stats.init(),
        };
    }

    pub fn deinit(self: *Searcher) void {
        self.alloc.free(self.stack);
    }

    pub inline fn clearStack(self: *Searcher) void {
        inline for (0..MaxDepth) |i| self.stack[i] = std.mem.zeroes(History);
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

    inline fn materialDraw(self: *const Searcher) bool {
        if (self.b.lines.v != 0) return false; // Queens/rooks still on the board
        if (self.b.pawns.v != 0) return false; // Pawns still on the board

        const white = self.b.w_pieces.popcount();
        const black = self.b.b_pieces.popcount();
        if (white <= 2 and black <= 2) return true; // Not enough pieces

        if ((white == 3 and black == 1) or (white == 1 and black == 3))
            return self.b.diags.v == 0;

        return false;
    }

    inline fn updatePv(self: *Searcher, move: tp.Move) void {
        const ply = self.b.hash_in - self.start_ply;

        const from = @min(MaxDepth - 1, self.stack[ply + 1].pv_size);

        for (0..from) |i| self.stack[ply].pv[i + 1] = self.stack[ply + 1].pv[i];
        self.stack[ply].pv_size = from + 1;
        self.stack[ply].pv[0] = move;
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

    pub fn quietSearch(self: *Searcher, a: i32, b: i32) !i32 {
        const ply = self.b.hash_in - self.start_ply;

        // Check for three fold repetition and 50 move rule
        if (self.historyDraw()) return drawVal();

        // Check for insufficient material
        if (self.materialDraw()) return drawVal();

        if (ply >= MaxDepth) return self.nn.output(self.b.side);

        // Mate distance pruning (These are the best possible vals at this ply)
        var alpha = @max(a, mateVal(ply));
        const beta = @min(b, -mateVal(ply + 1));
        if (alpha >= beta) return alpha;

        const gen = mv.Maker.init(self.b);

        const pv = b != a + 1;
        const inCheck = gen.checks > 0;

        // TT Probe
        const tte = tt.probe(self.b);
        const tte_fine = tte.reader != null and tte.usable;

        // Trust the tt entry if it's usable
        if (!pv and
            tte_fine and
            tte.reader.?.usable(alpha, beta))
            return tte.reader.?.val.score;

        const static = if (tte_fine) tte.reader.?.val.eval else self.nn.output(self.b.side);
        const eval = if (tte_fine and tte.reader.?.usable(static - 1, static))
            tte.reader.?.val.score
        else
            static;
        self.stack[ply].static = static;

        if (eval >= alpha) alpha = eval;
        if (alpha >= beta) return alpha;

        var pick = pi.Picker.init(.QuietTT, self, &gen, tte);
        defer pick.deinit();

        const futility = eval + ev.CentiPawn * 11;

        var bestMove: ?tp.Move = null;
        var bestScore = alpha;
        var foundMove = false;

        // Removing killer move
        self.stack[ply].killer = null;

        const lower_bound = alpha;
        const upper_bound = beta;

        var stage = pick.stage;
        while (try pick.nextMove()) |move| {
            self.stack[ply].stage = stage;
            self.stack[ply].hist_score = pick.current_hist;
            self.stack[ply].move = move;

            stage = pick.stage;
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
            self.nn.move(self.b, move, undo);

            const score = -try self.quietSearch(-beta, -alpha);

            self.nn.remove(self.b, move, undo);
            self.b.remove(move, undo);

            if (score > bestScore) {
                bestScore = score;

                if (score > alpha) {
                    alpha = score;
                    bestMove = move;

                    // if (pv) {
                    //     self.updatePv(move);
                    //     self.stack[ply + 1].pv_size = 0;
                    // }

                    if (score >= beta) break;
                }
            }
        }

        // Check and stalemate
        if (!foundMove and inCheck) return mateVal(ply);

        // TT insert
        tt.store(
            self.b,
            bestScore,
            static,
            0,
            lower_bound,
            upper_bound,
            bestMove,
            self.start_ply,
            tte,
        );

        return bestScore;
    }

    pub fn search(self: *Searcher, a: i32, b: i32, dep: i12, cutnode: bool) !i32 {
        if (self.thread.stopped) return error.NoTime;
        var depth = dep;

        const ply = self.b.hash_in - self.start_ply;
        if (depth <= 0 or ply >= MaxDepth) return self.quietSearch(a, b);

        // Check for three fold repetition and 50 move rule
        if (self.historyDraw()) return drawVal();

        // Check for insufficient material
        if (self.materialDraw()) return drawVal();

        // Mate distance pruning (These are the best possible vals at this ply)
        var alpha = @max(a, mateVal(ply));
        const beta = @min(b, -mateVal(ply + 1));
        if (alpha >= beta) return alpha;

        const gen = mv.Maker.init(self.b);

        const inCheck = gen.checks > 0;
        const pv = b != a + 1;
        const root = ply == 0;

        // TT Probe
        const tte = tt.probe(self.b);
        const tte_fine = tte.reader != null and tte.usable;
        const tte_move = tte_fine and tte.reader.?.val.typ != .Upper;

        // Trust the tt entry if it's usable
        if (!root and
            tte_fine and
            (!pv or (tte.reader.?.val.typ == .Exact)) and
            tte.reader.?.val.depth >= depth and
            tte.reader.?.usable(alpha, beta))
            return tte.reader.?.val.score;

        const static = if (tte_fine) tte.reader.?.val.eval else self.nn.output(self.b.side);
        const eval = if (tte_fine and tte.reader.?.usable(static - 1, static))
            tte.reader.?.val.score
        else
            static;
        self.stack[ply].static = static;
        const improving = !inCheck and
            (ply <= 1 or static > self.stack[ply - 2].static);

        // This is taken from Weiss (it says it's taken from Rebel there)
        if (!tte_move) {
            if (pv and depth >= 3) depth -= 1;
            if (cutnode and depth >= 8) depth -= 1;
        }

        // Pruning
        if (!inCheck and
            !root and
            !pv and
            self.stack[ply - 1].move != null and
            !isMate(beta))
        {
            if (!tte_move) {
                const futility = beta + ev.CentiPawn * 13 *
                    @divFloor(depth - @intFromBool(improving), 2);
                const razor = if (isMate(alpha))
                    alpha
                else
                    alpha - ev.CentiPawn * 11 * depth * depth;

                // Reverse futility pruning
                if (eval >= futility) return eval;

                // Razoring
                if (!improving and eval < razor) {
                    const score = try self.quietSearch(-alpha - 1, -alpha);
                    if (score < alpha) return if (isMate(score)) alpha else score;
                }
            }

            const R: i12 = 4 + @divFloor(depth, 4);
            const null_depth = if (R >= depth) 0 else depth - R;

            // Null move pruning
            if (eval >= beta and eval >= static and cutnode) {
                self.stack[ply].stage = null;
                self.stack[ply].hist_score = null;
                self.stack[ply].move = null;

                const undo = self.b.applyNull();
                const score = -try self.search(-beta, -beta + 1, null_depth, !cutnode);
                self.b.removeNull(undo);

                if (score >= beta) return if (isMate(score)) beta else score;
            }

            // Prob cut
            const probcut_add = ev.CentiPawn * 12 *
                (@intFromBool(!improving) + @divFloor(depth, 2));
            const probcut_beta = beta + probcut_add;
            if (depth >= 4 and eval >= probcut_beta) {
                var pick = pi.Picker.init(.QuietTT, self, &gen, tte);
                defer pick.deinit();

                var stage = pick.stage;
                while (try pick.nextMove()) |move| {
                    self.stack[ply].stage = stage;
                    self.stack[ply].hist_score = pick.current_hist;
                    self.stack[ply].move = move;
                    stage = pick.stage;

                    const undo = self.b.apply(move);
                    self.nn.move(self.b, move, undo);

                    var score = -try self.quietSearch(-probcut_beta, -probcut_beta + 1);

                    self.nn.remove(self.b, move, undo);
                    self.b.remove(move, undo);

                    if (score >= probcut_beta)
                        score = -try self.search(
                            -probcut_beta,
                            -probcut_beta + 1,
                            null_depth,
                            !cutnode,
                        );

                    if (score >= probcut_beta) return if (isMate(score))
                        score
                    else
                        score - probcut_add;
                }
            }
        }

        var pick = pi.Picker.init(.TT, self, &gen, tte);
        defer pick.deinit();

        const depth_sq = @as(i32, @intCast(depth)) * @as(i32, @intCast(depth));
        const quiet_count = if (improving)
            2 + depth_sq
        else
            @divFloor(depth_sq, 2);
        const tt_capture = tte_move and !self.b.isQuiet(tte.reader.?.val.move);

        var bestMove: ?tp.Move = null;
        var bestScore: i32 = -MateVal;

        var histories = try std.ArrayList(tp.Move).initCapacity(self.alloc, 64);
        defer histories.deinit();

        var move_counter: u8 = 0;

        // Removing killer move
        self.stack[ply].killer = null;

        const lower_bound = alpha;
        const upper_bound = beta;

        var stage = pick.stage;
        while (try pick.nextMove()) |move| {
            self.stack[ply].stage = stage;
            self.stack[ply].hist_score = pick.current_hist;
            self.stack[ply].move = move;
            stage = pick.stage;

            const undo = self.b.apply(move);
            self.nn.move(self.b, move, undo);

            var score: i32 = undefined;
            var next_depth = depth - 1;

            // Check extensions
            if (!root and ply <= 10 and inCheck) next_depth += 1;

            const quiet = undo.typ == null;

            // Reductions
            const min_count = @as(u3, @intFromBool(pv)) +
                @as(u3, @intFromBool(!quiet)) +
                @as(u3, @intFromBool(self.stack[ply].stage == .TT));
            if (!isMate(bestScore) and !root and move_counter > min_count) {
                var R: i12 = 0;

                // LMR
                const depth_index: u5 = @intCast(@min(31, depth));
                const ply_index: u5 = @intCast(@min(31, ply));
                R += Reductions[@intFromBool(quiet)][depth_index][ply_index];
                if (pick.current_hist) |h|
                    R -= @intCast(std.math.clamp(@divFloor(h, hi.CentiHist), -2, 2));

                if (cutnode) R += 2;

                if (quiet and
                    move_counter > quiet_count)
                    R += 2;

                if (tt_capture) R += 1;

                // Various pruning reduction criteria
                if (pv) R -= 1;
                if (inCheck) R -= 1;
                if (self.stack[ply].stage == .Killer) R -= 1;
                if (improving) R -= 1;

                if (R < 0) R = 0;
                const r_depth = if (R >= next_depth) @min(next_depth, 1) else next_depth - R;

                score = -try self.search(-alpha - 1, -alpha, r_depth, true);

                if (score > alpha and r_depth < next_depth)
                    score = -try self.search(-alpha - 1, -alpha, next_depth, !cutnode);
            } else if (!pv or move_counter > 0)
                score = -try self.search(-alpha - 1, -alpha, next_depth, !cutnode);

            if (pv and (move_counter == 0 or score > alpha))
                score = -try self.search(-beta, -alpha, next_depth, false);

            self.nn.remove(self.b, move, undo);
            self.b.remove(move, undo);

            try histories.append(move);
            move_counter += 1;
            if (score > bestScore) {
                bestScore = score;

                if (score > alpha) {
                    alpha = score;
                    bestMove = move;

                    if (pv) {
                        self.updatePv(move);
                        self.stack[ply + 1].pv_size = 0;
                    }

                    if (score >= beta) {
                        self.stats.update(
                            self.b,
                            &histories,
                            if (root) null else self.stack[ply - 1].move,
                            depth,
                            move_counter,
                            @divFloor(score - eval, ev.CentiPawn),
                        );
                        if (ply > 0) self.stack[ply - 1].killer = move;
                        break;
                    }
                }
            }
        }

        // Check and stalemate
        if (move_counter == 0) return if (inCheck) mateVal(ply) else drawVal();

        // TT insert
        tt.store(
            self.b,
            bestScore,
            static,
            depth,
            lower_bound,
            upper_bound,
            bestMove,
            self.start_ply,
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

            if (score <= alpha) {
                beta = alpha + @divFloor(beta - alpha, 2) + 1;
                alpha -= delta;
                if (alpha < -MateVal) alpha = -MateVal;
            } else if (score >= beta) {
                beta += delta;
                if (beta > MateVal) beta = MateVal;
            } else return score;

            delta += @divFloor(delta, 3);
            if (delta > MateVal) delta = MateVal;
        }
    }

    pub fn iterDeepening(self: *Searcher) !void {
        var depth: f32 = self.thread.iter;
        var score: i32 = 0;
        while (true) {
            const se_depth: i12 = @intFromFloat(depth + 0.5);

            // If we don't have a PV, we just do a standard search
            if (!self.pv_exists) {
                score = self.search(-MateVal, MateVal, se_depth, false) catch |err| {
                    switch (err) {
                        error.NoTime => break,
                        else => return err,
                    }
                };

                self.pv_exists = self.stack[0].pv_size != 0;
            }

            if (self.pv_exists) {
                score = self.aspiration(score, se_depth) catch |err| {
                    switch (err) {
                        error.NoTime => break,
                        else => return err,
                    }
                };
            }

            // Update the threads result
            if (self.pv_exists) {
                self.thread.res = po.Result{
                    .pv = self.stack[0].pv,
                    .pv_size = self.stack[0].pv_size,
                    .score = score,
                    .depth = se_depth,
                };
                depth += self.thread.iter;
            }

            self.clearStack();
            if (isMate(score)) break;
        }
    }
};
