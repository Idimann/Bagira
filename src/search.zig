const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const mv = @import("movegen.zig");
const nn = @import("nn.zig");
const ev = @import("eval.zig");
const tt = @import("tt.zig");
const po = @import("pool.zig");
const pi = @import("movepick.zig");
const hi = @import("history.zig");
const see = @import("see.zig");

fn initReductions() [2][32][64]i12 {
    @setEvalBranchQuota(2048);
    var ret = std.mem.zeroes([2][32][64]i12);

    for (1..32) |i| {
        for (1..64) |j| {
            const log = @log(@as(f64, @floatFromInt(i))) * @log(@as(f64, @floatFromInt(j)));

            ret[0][i][j] = @intFromFloat(0.38 + log / 3.76);
            ret[1][i][j] = @intFromFloat(2.01 + log / 2.32);
        }
    }

    return ret;
}

fn initPrunes() [2][32]i32 {
    var ret = std.mem.zeroes([2][32]i32);

    for (1..32) |i| {
        const depth: comptime_float = @floatFromInt(i);

        ret[0][i] = @intFromFloat(-13.78 * depth * depth);
        ret[1][i] = @intFromFloat(-108.35 * depth);
    }

    return ret;
}

const Reductions = initReductions();
const Prunes = initPrunes();

pub const MaxDepth = 255;
const History = struct {
    static: i32,
    stage: ?pi.Stage,
    pv: [MaxDepth]tp.Move,
    pv_size: u8,
    killer: ?tp.Move,
    move: ?tp.Move,
    hist_score: ?i32,
    excluded: ?tp.Move,
    in_check: bool,
};

pub const Searcher = struct {
    alloc: std.mem.Allocator,
    b: *bo.Board,
    nnw: *nn.NN,
    stack: []History,
    start_ply: u12,
    thread: *po.Thread,

    stats: hi.Stats,
    corrections: hi.Corrections,

    // The shift is for overflow protection
    pub const MateVal = std.math.maxInt(i32) >> 16;

    pub fn init(thread: *po.Thread, alloc: std.mem.Allocator) !Searcher {
        const stack = try alloc.alloc(History, MaxDepth);
        inline for (0..MaxDepth) |i| stack[i] = std.mem.zeroes(History);

        return .{
            .alloc = alloc,
            .b = &thread.board,
            .nnw = &thread.nnw,
            .stack = stack,
            .start_ply = thread.board.hash_in,
            .thread = thread,
            .stats = hi.Stats.init(),
            .corrections = hi.Corrections.init(),
        };
    }

    pub fn deinit(self: *Searcher) void {
        self.alloc.free(self.stack);
    }

    pub inline fn clearStack(self: *Searcher) void {
        inline for (0..MaxDepth) |i| self.stack[i] = std.mem.zeroes(History);
    }

    inline fn historyDraw(self: *const Searcher, root: bool) bool {
        // 50 move rule
        if (self.b.move_rule >= 100) return true;

        // Repetition
        if (self.b.hash_in > 1) {
            var iter: u8 = 2;
            var once = false;

            while (iter <= self.b.move_rule) {
                if (self.b.hash[self.b.hash_in - iter] == self.b.getHash()) {
                    if (!root or once)
                        return true
                    else
                        once = true;
                }

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

    inline fn stalemateDanger(self: *const Searcher) bool {
        return self.b.w_pieces.popcount() +
            self.b.b_pieces.popcount() -
            self.b.typePieces(.Pawn).popcount() <= 1;
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

    inline fn isLoss(val: i32) bool {
        const mate = comptime mateVal(std.math.maxInt(u12));
        return val <= mate;
    }

    inline fn isWin(val: i32) bool {
        const mate = comptime mateVal(std.math.maxInt(u12));
        return val >= -mate;
    }

    pub inline fn isMate(val: i32) bool {
        return isWin(val) or isLoss(val);
    }

    inline fn evaluate(self: *Searcher) i32 {
        const mate = comptime mateVal(std.math.maxInt(u12));
        const score = self.nnw.output(self.b);

        const ply = self.b.hash_in - self.start_ply;
        const prev = if (ply >= 2) self.stack[ply - 2].move else null;
        const move = if (ply >= 1) self.stack[ply - 1].move else null;
        const corr = @divFloor(self.corrections.get(self.b, prev, move), 12);

        return std.math.clamp(score + corr, mate + 1, -mate - 1);
    }

    pub fn quietSearch(self: *Searcher, a: i32, b: i32) !i32 {
        const ply = self.b.hash_in - self.start_ply;
        if (ply == 0) self.b.print_detailed();

        // Check for three fold repetition and 50 move rule
        if (self.historyDraw(false)) return drawVal();

        // Check for insufficient material
        if (self.materialDraw()) return drawVal();

        if (ply >= MaxDepth) return ev.adjust(self.evaluate(), self.b);

        // Mate distance pruning (These are the best possible vals at this ply)
        var alpha = @max(a, mateVal(ply));
        const beta = @min(b, -mateVal(ply + 1));
        if (alpha >= beta) return alpha;

        const gen = mv.Maker.init(self.b);
        const pawn_attacked = gen.attackedPawn();

        const pv = b != a + 1;
        self.stack[ply].in_check = gen.checks > 0;

        // TT Probe
        const tte = tt.probe(self.b);
        const tte_fine = tte.reader != null and tte.usable;
        const tte_move = tte_fine and tte.reader.?.val.typ != .Upper;
        var tte_score = if (tte_fine) tte.reader.?.val.score else 0;
        const hash_move: ?tp.Move = if (tte_move)
            tte.reader.?.val.move
        else
            null;
        // Fix TT mate distance
        if (tte_fine and isMate(tte_score)) {
            if (isLoss(tte_score))
                tte_score += ply
            else
                tte_score -= ply;
        }

        // Trust the tt entry if it's usable
        if (self.b.move_rule < 90 and
            !pv and
            tte_fine and
            tte.reader.?.usable(tte_score, alpha, beta))
            return tte_score;

        const static = ev.adjust(
            if (tte_fine) tte.reader.?.val.eval else self.evaluate(),
            self.b,
        );
        const eval = if (tte_fine and tte.reader.?.usable(tte_score, static - 1, static))
            tte_score
        else
            static;
        self.stack[ply].static = static;

        if (!self.stack[ply].in_check and eval >= alpha) {
            alpha = eval;
            if (pv) {
                self.stack[ply].pv_size = 0;
                self.stack[ply + 1].pv_size = 0;
            }
        }
        if (alpha >= beta) return alpha;

        var pick = pi.Picker.init(.QuietSearchTT, self, &gen, hash_move, pawn_attacked, null);
        defer pick.deinit();

        var best_move: ?tp.Move = null;
        var best_score = alpha;

        // Constants for pruning
        const futility = eval + ev.CentiPawn * 8;

        // Removing killer move
        self.stack[ply].killer = null;

        var move_counter: u8 = 0;
        const lower_bound = alpha;
        const upper_bound = beta;

        while (try pick.nextMove()) |move| {
            self.stack[ply].stage = pick.ret_stage;
            self.stack[ply].hist_score = pick.current_val;
            self.stack[ply].move = move;
            move_counter += 1;

            // Pruning
            const followup = self.stack[ply - 1].move != null and
                self.stack[ply - 1].move.?.to == move.to;
            if (!isLoss(best_score) and !followup) {
                // Move count pruning
                if (!move.typ.promotion() and move_counter > 2) continue;

                // Futility pruning
                if (!self.stack[ply].in_check and !self.b.isQuiet(move)) {
                    if (futility +
                        ev.PieceValue[@intFromEnum(self.b.pieceType(move.to))] <= alpha)
                        continue;
                }

                // SEE
                if (!see.see(self.b, move, &gen, 0)) continue;
            }

            const undo = self.b.apply(move);
            tt.prefetch(self.b, false);
            self.nnw.move(self.b, move, undo);

            const score = -try self.quietSearch(-beta, -alpha);

            self.nnw.remove(self.b, move, undo);
            self.b.remove(move, undo);

            if (score > best_score) {
                best_score = score;

                if (score > alpha) {
                    alpha = score;
                    best_move = move;

                    if (pv) {
                        self.updatePv(move);
                        self.stack[ply + 1].pv_size = 0;
                    }

                    if (score >= beta) break;
                }
            }
        }

        // Check and stalemate
        if (move_counter == 0 and self.stack[ply].in_check) {
            if (self.stack[ply].excluded != null) return alpha;

            return drawVal();
        }

        // TT insert
        if (self.stack[ply].excluded == null) {
            var store_score = best_score;

            // Fix inserting mate distance
            if (isMate(store_score)) {
                if (isLoss(store_score))
                    store_score -= ply
                else
                    store_score += ply;
            }

            tt.store(
                self.b,
                store_score,
                static,
                0,
                lower_bound,
                upper_bound,
                best_move,
                self.start_ply,
                tte,
            );
        }

        return best_score;
    }

    pub fn search(self: *Searcher, a: i32, b: i32, dep: i12, cutnode: bool) !i32 {
        @setEvalBranchQuota(2048);

        if (self.thread.stopped) return error.NoTime;
        var depth = dep;

        const ply = self.b.hash_in - self.start_ply;
        if (depth <= 0 or ply >= MaxDepth) return self.quietSearch(a, b);

        // Check for three fold repetition and 50 move rule
        if (self.historyDraw(ply == 0)) return drawVal();

        // Check for insufficient material
        if (self.materialDraw()) return drawVal();

        // Mate distance pruning (These are the best possible vals at this ply)
        var alpha = @max(a, mateVal(ply));
        const beta = @min(b, -mateVal(ply + 1));
        if (alpha >= beta) return alpha;

        const gen = mv.Maker.init(self.b);
        const pawn_attacked = gen.attackedPawn();

        self.stack[ply].in_check = gen.checks > 0;
        const pv = b != a + 1;
        const root = ply == 0;
        self.thread.nodes += 1;

        // TT Probe
        const tte = tt.probe(self.b);
        const tte_fine = tte.reader != null and tte.usable;
        const tte_move = tte_fine and tte.reader.?.val.typ != .Upper;
        var tte_score = if (tte_fine) tte.reader.?.val.score else 0;
        const hash_move: ?tp.Move = if (root)
            self.thread.best_root.pv[0]
        else if (tte_move)
            tte.reader.?.val.move
        else
            null;

        // Fix TT mate distance
        if (tte_fine and isMate(tte_score)) {
            if (isLoss(tte_score))
                tte_score += ply
            else
                tte_score -= ply;
        }

        // Trust the tt entry if it's usable
        if (self.b.move_rule < 90 and
            !pv and
            !root and
            self.stack[ply].excluded == null and
            tte_fine and
            tte.reader.?.val.depth >= depth and
            tte.reader.?.usable(tte_score, alpha, beta))
            return tte_score;

        const static = ev.adjust(
            if (tte_fine) tte.reader.?.val.eval else self.evaluate(),
            self.b,
        );
        const eval = if (tte_fine and tte.reader.?.usable(tte_score, static - 1, static))
            tte_score
        else
            static;
        self.stack[ply].static = static;
        const improving = ply <= 1 or
            (!self.stack[ply].in_check and
                !self.stack[ply - 2].in_check and
                static > self.stack[ply - 2].static);

        // This is taken from Weiss (it says it's taken from Rebel there)
        if (!tte_move) {
            if (pv and depth >= 3) depth -= 1;
            if (cutnode and depth >= 8) depth -= 1;
        }

        // Real Pruning
        if (!self.stack[ply].in_check and
            !root and
            !pv and
            self.stack[ply].excluded == null and
            self.stack[ply - 1].move != null and
            !isLoss(beta))
        {
            if (!tte_move) {
                const futility = beta + ev.CentiPawn * 13 *
                    @divFloor(depth - @intFromBool(improving), 2);
                const razor = if (isLoss(alpha))
                    alpha
                else
                    alpha - ev.CentiPawn * 14 * depth * depth;

                // Reverse futility pruning (Depth for mate finding)
                if (!isWin(eval) and depth < 15 and eval >= futility)
                    return beta + @divFloor(eval - beta, 3);

                // Razoring
                if (!improving and eval < razor)
                    return try self.quietSearch(alpha, beta);
            }

            const R: i12 = 4 + @divFloor(depth, 4);
            const null_depth = if (R >= depth) 0 else depth - R;

            // Null move pruning
            if (eval >= beta and
                static >= beta - ev.CentiPawn * depth + 2 * ev.PawnBase and
                cutnode and
                !self.stalemateDanger())
            {
                self.stack[ply].stage = null;
                self.stack[ply].hist_score = null;
                self.stack[ply].move = null;

                const undo = self.b.applyNull();
                tt.prefetch(self.b, false);
                const score = -try self.search(-beta, -beta + 1, null_depth, !cutnode);
                self.b.removeNull(undo);

                if (score >= beta) return if (isMate(score)) beta else score;
            }

            // Prob cut
            const improve_int: i32 = @intCast(@intFromBool(improving));
            const probcut_add = ev.CentiPawn * (8 - 2 * improve_int);
            const probcut_beta = beta + probcut_add;
            if (depth >= 3 and (!tte_fine or eval >= probcut_beta)) {
                var pick = pi.Picker.init(
                    .ProbCutTT,
                    self,
                    &gen,
                    hash_move,
                    pawn_attacked,
                    probcut_beta - static,
                );
                defer pick.deinit();

                var stage = pick.stage;
                while (try pick.nextMove()) |move| {
                    self.stack[ply].stage = stage;
                    self.stack[ply].hist_score = pick.current_val;
                    self.stack[ply].move = move;
                    stage = pick.stage;

                    const undo = self.b.apply(move);
                    tt.prefetch(self.b, false);
                    self.nnw.move(self.b, move, undo);

                    var score = -try self.quietSearch(-probcut_beta, -probcut_beta + 1);

                    if (score >= probcut_beta)
                        score = -try self.search(
                            -probcut_beta,
                            -probcut_beta + 1,
                            null_depth,
                            !cutnode,
                        );

                    self.nnw.remove(self.b, move, undo);
                    self.b.remove(move, undo);

                    if (score >= probcut_beta) {
                        if (isMate(score)) return score;

                        // Store ProbCut data
                        tt.store(
                            self.b,
                            score - probcut_add,
                            static,
                            depth,
                            probcut_beta - 1,
                            probcut_beta,
                            move,
                            self.start_ply,
                            tte,
                        );
                        return score - probcut_add;
                    }
                }
            }
        }

        var pick = pi.Picker.init(.TT, self, &gen, hash_move, pawn_attacked, null);
        defer pick.deinit();

        // Constants for pruning
        const depth_sq = @as(i32, @intCast(depth)) * @as(i32, @intCast(depth));
        const futility = eval + ev.CentiPawn * 11;
        const quiet_max = @divFloor(3 + depth_sq, 2 - @as(u2, @intFromBool(improving)));

        // Constants for LMR
        const tt_capture = tte_move and (!self.b.isQuiet(hash_move.?) or
            hash_move.?.typ.promotion());

        var best_move: ?tp.Move = null;
        var best_score: i32 = -MateVal;

        var histories = try std.ArrayList(tp.Move).initCapacity(self.alloc, 64);
        defer histories.deinit();

        // Removing killer move
        self.stack[ply].killer = null;

        var move_counter: u8 = 0;
        const lower_bound = alpha;
        const upper_bound = beta;

        while (try pick.nextMove()) |move| {
            // Skip the excluded move
            if (self.stack[ply].excluded != null and
                move.equals(self.stack[ply].excluded.?)) continue;

            const start_nodes = self.thread.nodes;

            self.stack[ply].stage = pick.ret_stage;
            self.stack[ply].hist_score = pick.current_val;
            self.stack[ply].move = move;
            move_counter += 1;

            var next_depth = depth - 1;
            var score: i32 = undefined;

            const quiet = self.b.isQuiet(move);
            var r_depth = next_depth;
            if (!isLoss(best_score)) {
                var R: i12 = 0;
                const depth_index: u5 = @intCast(@min(31, depth));
                const move_index: u6 = @intCast(@min(63, move_counter));
                R += Reductions[@intFromBool(quiet)][depth_index][move_index];
                if (pick.current_val) |h|
                    R -= @intCast(std.math.clamp(@divFloor(h, 4 * hi.CentiHist), -3, 2));

                // Reduce more
                if (cutnode) {
                    R += 2;
                    R += @intFromBool(tte_move);
                }
                if (tt_capture) R += 1;
                if (tte_fine and tte.reader.?.val.typ == .Exact) R += 1;
                if (!improving) R += 1;

                // We do wanna reduce these more
                if (pv) R -= 1;
                R -= switch (self.stack[ply].stage.?) {
                    .TT => 2,
                    .GoodCaptures => 1,
                    .Killer => 1,
                    .BadCaptures => 0,
                    .Quiets => 0,
                    else => unreachable,
                };
                R = @min(@max(next_depth - 1, 0), @max(R, 1));
                r_depth = next_depth - R;
            }

            // Pruning
            if (!root and !isLoss(best_score) and !self.stalemateDanger()) {
                // Move count pruning
                if (move_counter >= quiet_max)
                    pick.skip_quiets = true;

                // Futility pruning
                if (!self.b.isQuiet(move)) {
                    if (r_depth < 7 and !self.stack[ply].in_check) {
                        const futil_val = futility + 8 * ev.CentiPawn * r_depth +
                            ev.PieceValue[@intFromEnum(self.b.pieceType(move.to))];
                        if (futil_val <= alpha) continue;
                    }
                } else {
                    if (r_depth < 12 and !self.stack[ply].in_check) {
                        const futil_val = futility + 6 * ev.CentiPawn * r_depth;
                        if (futil_val <= alpha) {
                            if (!isMate(best_score) and
                                !isMate(futil_val) and
                                best_score < futil_val)
                                best_score = futil_val;
                            continue;
                        }
                    }
                }

                // SEE pruning
                const followup = ply >= 1 and
                    self.stack[ply - 1].move != null and
                    self.stack[ply - 1].move.?.to == move.to;
                if (!followup and !see.see(
                    self.b,
                    move,
                    &gen,
                    Prunes[@intFromBool(!quiet)][@intCast(r_depth)],
                )) continue;
            }

            // Extensions
            var E: i12 = 0;
            if (!root and self.stack[ply].excluded == null) {
                // Singular extension and multi cut
                if (depth >= 6 + @as(i12, @intCast(@intFromBool(pv))) and
                    self.stack[ply].stage == .TT and
                    tte.reader.?.val.depth + 3 >= depth and
                    tte.reader.?.val.typ == .Lower and
                    !isMate(tte_score))
                {
                    const pv_int: i32 = @intCast(@intFromBool(pv));
                    const depth_int = @as(i32, @intCast(depth)) * (2 - pv_int);
                    const sing = tte_score - depth_int;

                    self.stack[ply].excluded = move;
                    score = try self.search(
                        sing - 1,
                        sing,
                        @divFloor(next_depth, 2),
                        !cutnode,
                    );
                    self.stack[ply].excluded = null;

                    if (score < sing)
                        E += 1
                    else if (sing >= beta)
                        return sing
                    else if (tte_score >= beta)
                        E -= 3 - @as(i12, @intCast(@intFromBool(pv)))
                    else if (cutnode)
                        E -= 2
                    else if (tte_score <= alpha)
                        E -= 1;
                }
            }
            next_depth += @max(E, -@divFloor(next_depth, 3));

            const undo = self.b.apply(move);
            tt.prefetch(self.b, false);
            self.nnw.move(self.b, move, undo);

            // LMR
            if (next_depth > 1 and
                !isLoss(best_score) and
                !root and
                move_counter > 1)
            {
                score = -try self.search(-alpha - 1, -alpha, r_depth, true);

                if (score > alpha and r_depth < next_depth)
                    score = -try self.search(-alpha - 1, -alpha, next_depth, !cutnode);
            } else if (!pv or move_counter > 1)
                score = -try self.search(-alpha - 1, -alpha, next_depth, !cutnode);

            if (pv and (move_counter == 1 or score > alpha))
                score = -try self.search(-beta, -alpha, next_depth, false);

            self.nnw.remove(self.b, move, undo);
            self.b.remove(move, undo);

            if (root) {
                var rm: *po.RootMove = &self.thread.root_moves.items[0];
                var index: usize = 0;
                for (0..self.thread.root_moves.items.len) |i| {
                    if (self.thread.root_moves.items[i].move.equals(move)) {
                        rm = &self.thread.root_moves.items[i];
                        index = i;
                        break;
                    }
                }

                rm.nodes += self.thread.nodes - start_nodes;
                if (!isMate(rm.avg_score)) rm.avg_score = @divFloor(rm.avg_score + score, 2);
                const abs_score: i32 = @intCast(@abs(score));
                if (!isMate(rm.avg_score_sq)) {
                    rm.avg_score_sq = if (isMate(score))
                        score
                    else
                        @divFloor(rm.avg_score_sq + score * abs_score, 2);
                }

                if (move_counter == 1 or score > alpha) {
                    rm.score = score;
                    rm.depth = depth;
                    rm.pv_size = self.stack[1].pv_size;

                    const from = @min(MaxDepth - 1, self.stack[ply + 1].pv_size);
                    for (0..from) |i| rm.pv[i] = self.stack[ply + 1].pv[i];
                } else rm.score = -MateVal;
            }

            try histories.append(move);
            if (score > best_score) {
                best_score = score;

                if (score > alpha) {
                    alpha = score;
                    best_move = move;

                    if (pv) {
                        self.updatePv(move);
                        self.stack[ply + 1].pv_size = 0;
                    }

                    if (score >= beta) {
                        if (ply >= 1) self.stack[ply - 1].killer = move;
                        break;
                    }
                }
            }
        }

        // Check and stalemate
        if (move_counter == 0) {
            if (self.stack[ply].excluded != null) return alpha;

            return if (self.stack[ply].in_check) mateVal(ply) else drawVal();
        }

        if (best_move != null) {
            self.stats.update(
                self.b,
                best_move.?,
                &histories,
                if (root) null else self.stack[ply - 1].move,
                depth,
            );
        }

        // Update Corrections
        const prev = if (ply >= 2) self.stack[ply - 2].move else null;
        const move = if (ply >= 1) self.stack[ply - 1].move else null;
        if (!self.stack[ply].in_check and
            (best_move == null or self.b.isQuiet(best_move.?)) and
            !(best_score >= beta and best_score <= static) and
            !(best_move == null and best_score >= static))
        {
            self.corrections.update(self.b, prev, move, depth, best_score, static);
        }

        // TT insert
        if (self.stack[ply].excluded == null and !root) {
            var store_score = best_score;

            // Fix inserting mate distance
            if (isMate(store_score)) {
                if (isLoss(store_score))
                    store_score -= ply
                else
                    store_score += ply;
            }

            tt.store(
                self.b,
                store_score,
                static,
                depth,
                lower_bound,
                upper_bound,
                best_move,
                self.start_ply,
                tte,
            );
        }

        return best_score;
    }

    pub fn aspiration(self: *Searcher, prev: i32, depth: i12) !i32 {
        const avg_sq: i32 = @intCast(@abs(self.thread.best_root.avg_score_sq));
        var delta = @divFloor(ev.CentiPawn, 8) + @divFloor(avg_sq, ev.CentiPawn * 16);
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
        var score: i32 = -MateVal;
        while (true) {
            const se_depth: i12 = @intFromFloat(depth + 0.5);
            // For the start, we just do a normal search
            if (score == -MateVal) {
                score = self.search(-MateVal, MateVal, se_depth, false) catch |err| {
                    switch (err) {
                        error.NoTime => break,
                        else => return err,
                    }
                };
            } else {
                score = self.aspiration(
                    self.thread.best_root.avg_score,
                    se_depth,
                ) catch |err| {
                    switch (err) {
                        error.NoTime => break,
                        else => return err,
                    }
                };
            }
            self.thread.sortRootMoves();
            depth += self.thread.iter;

            self.clearStack();
            if (isMate(score)) break;
        }
    }
};
