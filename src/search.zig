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

pub const MaxDepth = 255;
fn initLmr() [MaxDepth][64]i12 {
    @setEvalBranchQuota(MaxDepth * 64);
    var ret = std.mem.zeroes([MaxDepth][64]i12);

    for (1..MaxDepth) |i| {
        for (1..64) |j| {
            const log = @log(@as(f64, @floatFromInt(i))) * @log(@as(f64, @floatFromInt(j)));
            ret[i][j] = @intFromFloat(0.93 + log * 0.47);
        }
    }

    return ret;
}

fn initSeeCuts() [2][MaxDepth]i32 {
    var ret = std.mem.zeroes([2][MaxDepth]i32);

    for (1..MaxDepth) |i| {
        const depth: i32 = @intCast(i);

        ret[0][i] = -ev.CentiPawn * depth * depth;
        ret[1][i] = -ev.CentiPawn * 6 * depth;
    }

    return ret;
}

fn initLmp() [2][MaxDepth]i32 {
    var ret = std.mem.zeroes([2][MaxDepth]i32);

    for (1..MaxDepth) |i| {
        const depth: comptime_float = @floatFromInt(i);

        ret[0][i] = @intFromFloat(1.43 + 0.33 * depth * depth);
        ret[1][i] = @intFromFloat(1.77 + 0.98 * depth * depth);
    }

    return ret;
}

const LMR = initLmr();
const SEE_CUTS = initSeeCuts();
const LMP = initLmp();

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
    nmp_ply: u12,

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
            .nmp_ply = 0,
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
        if (alpha >= beta) return if (isMate(alpha)) alpha else @divTrunc(alpha + beta, 2);

        var pick = pi.Picker.init(.QuietSearchTT, self, &gen, hash_move, pawn_attacked, null);
        defer pick.deinit();

        var best_move: ?tp.Move = null;
        var best_score = alpha;

        // Constants for pruning
        const futility = eval + ev.CentiPawn * 9;

        // Removing killer move
        self.stack[ply].killer = null;

        var move_counter: u8 = 0;
        const lower_bound = alpha;
        const upper_bound = beta;

        while (try pick.nextMove()) |move| {
            self.stack[ply].stage = pick.ret_stage;
            self.stack[ply].hist_score = if (pick.current_val) |v| v.hist else null;
            self.stack[ply].move = move;
            move_counter += 1;

            // Pruning
            if (!isLoss(best_score)) {
                const followup = self.stack[ply - 1].move != null and
                    self.stack[ply - 1].move.?.to == move.to;

                if (!self.stack[ply].in_check and
                    !followup and
                    !move.typ.promotion())
                {
                    // Move count pruning
                    if (move_counter > 2) continue;

                    // Futility pruning
                    if (!self.stack[ply].in_check) {
                        if (futility +
                            ev.PieceValue[@intFromEnum(self.b.pieceType(move.to))] <= alpha)
                            continue;
                    }

                    // SEE
                    if (!see.see(self.b, move, &gen, alpha - futility)) {
                        best_score = @min(alpha, futility);
                        continue;
                    }
                }

                if (!see.see(self.b, move, &gen, -ev.CentiPawn * 3)) continue;
            }

            const undo = self.b.apply(move);
            tt.prefetch(self.b, false);
            self.nnw.apply(self.b, move, undo);

            const score = -try self.quietSearch(-beta, -alpha);

            self.nnw.remove();
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

        if (!isMate(best_score) and best_score > beta)
            best_score = @divTrunc(best_score + beta, 2);

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
        const tte_pv = tte_fine and tte.reader.?.val.typ == .Exact;

        var tte_score = if (tte_fine) tte.reader.?.val.score else 0;
        const hash_move: ?tp.Move = if (root)
            self.thread.best_root.move
        else if (tte_move)
            tte.reader.?.val.move
        else
            null;
        const tte_depth: i12 = if (tte_fine) @intCast(tte.reader.?.val.depth) else 0;

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
            tte_depth >= depth and
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
        if (!self.stack[ply].in_check and !root and !pv) {
            if (!tte_move or tte_score < alpha - ev.CentiPawn * 6) {
                const futility = ev.CentiPawn * 4 * depth -
                    (if (improving) ev.CentiPawn * 9 else 0);
                const razor = if (isLoss(alpha))
                    alpha
                else
                    alpha - ev.CentiPawn * 12 * depth * depth;

                // Reverse futility pruning (Depth for mate finding)
                if (depth < 15 and
                    !isWin(eval) and
                    !isLoss(beta) and
                    eval >= beta and
                    eval >= beta + futility)
                    return beta + @divFloor(eval - beta, 3);

                // Razoring
                if (eval < razor)
                    return try self.quietSearch(alpha, beta);
            }

            // Null move pruning
            if (depth >= 4 and
                !isLoss(beta) and
                self.stack[ply].excluded == null and
                self.stack[ply - 1].move != null and
                eval >= beta and
                ply >= self.nmp_ply and
                !self.stalemateDanger())
            {
                const null_depth = @max(depth -
                    4 -
                    @divFloor(depth, 4) -
                    @as(i12, @intCast(@min(@divFloor(eval - beta, ev.CentiPawn * 8), 5))), 0);
                self.stack[ply].stage = null;
                self.stack[ply].hist_score = null;
                self.stack[ply].move = null;

                const undo = self.b.applyNull();
                tt.prefetch(self.b, false);
                const score = -try self.search(-beta, -beta + 1, null_depth, !cutnode);
                self.b.removeNull(undo);

                if (score >= beta) {
                    // Verification search
                    var ver = score;
                    if (self.nmp_ply == 0 and (depth > 16 or isMate(score))) {
                        self.nmp_ply = @intCast(ply + 3 * @divFloor(null_depth, 4));
                        ver = try self.search(beta - 1, beta, null_depth, !cutnode);
                        self.nmp_ply = 0;
                    }

                    if (ver >= beta) return if (isWin(score)) beta else score;
                }
            }

            // Prob cut
            const improve_int: i32 = @intCast(@intFromBool(improving));
            const probcut_add = ev.CentiPawn * (8 - 2 * improve_int);
            const probcut_beta = beta + probcut_add;
            if (depth >= 5 and
                !isMate(beta) and
                self.stack[ply].excluded == null and
                !(tte_fine and
                    tte_depth >= depth - 3 and
                    tte_score < probcut_beta))
            {
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
                    self.stack[ply].hist_score = if (pick.current_val) |v| v.hist else null;
                    self.stack[ply].move = move;
                    stage = pick.stage;

                    const undo = self.b.apply(move);
                    tt.prefetch(self.b, false);
                    self.nnw.apply(self.b, move, undo);

                    var score = -try self.quietSearch(-probcut_beta, -probcut_beta + 1);

                    if (score >= probcut_beta and depth > 5)
                        score = -try self.search(
                            -probcut_beta,
                            -probcut_beta + 1,
                            depth - 5,
                            !cutnode,
                        );

                    self.nnw.remove();
                    self.b.remove(move, undo);

                    if (score >= probcut_beta) {
                        // if (isMate(score)) return score;
                        // return score - probcut_add;
                        return score;
                    }
                }
            }
        }

        var pick = pi.Picker.init(.TT, self, &gen, hash_move, pawn_attacked, null);
        defer pick.deinit();

        // Constants for pruning
        const futility = static + ev.CentiPawn * 5;

        // Constants for LMR
        const tt_capture = tte_move and self.b.isNoisy(hash_move.?);

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
            self.stack[ply].hist_score = if (pick.current_val) |v| v.hist else null;
            self.stack[ply].move = move;
            move_counter += 1;

            var next_depth = depth - 1;
            var score: i32 = undefined;

            var r_depth = next_depth;
            if (!root and !isLoss(best_score)) {
                const depth_index: u8 = @intCast(@min(MaxDepth - 1, depth));
                const move_index: u6 = @intCast(@min(63, move_counter));
                var R = LMR[depth_index][move_index];

                // Increase reduction if not improving
                if (!improving) R += 1;

                // Adjust reduction based on history
                if (self.stack[ply].hist_score) |h|
                    R -= @intCast(@divTrunc(
                        h - ev.CentiPawn * hi.CentiHist * 10,
                        8 * ev.CentiPawn * hi.CentiHist,
                    ));

                // Increase reduction for cutnodes
                if (cutnode) {
                    R += 2;
                    R += @intFromBool(tte_move);
                }

                // Increase reduction if tt move exists and is a capture
                if (tt_capture) {
                    R += 1;
                    R += @intFromBool(depth < 8);
                }

                // We save some depth for a probable research
                if (!pv and tte_pv and self.stack[ply].excluded == null)
                    R += @min(depth - tte_depth, 2);

                // Increase reduction if there are cut offs at the next ply
                if (self.stack[ply].killer != null) R += 1;

                // Decrease reduction in pv nodes
                if (pv) R -= 1;

                // Adjust reduction based on move type
                R -= switch (self.stack[ply].stage.?) {
                    .TT => 2,
                    .GoodCaptures, .Killer => 1,
                    .GoodQuiets, .BadCaptures => 0,
                    .BadQuiets => -1,
                    else => unreachable,
                };
                R = @min(@max(next_depth - 1, 0), @max(R, 1));
                r_depth = next_depth - R;
            }

            // Pruning
            if (!root and !isLoss(best_score) and !self.stalemateDanger()) {
                // Move count pruning
                const depth_index: u8 = @intCast(@min(MaxDepth - 1, depth));
                if (move_counter >= LMP[@intFromBool(improving)][depth_index])
                    pick.skip_quiets = true;

                // Futility pruning
                if (self.b.isCapture(move)) {
                    if (r_depth < 8 and !self.stack[ply].in_check) {
                        const futil_val = futility + 8 * ev.CentiPawn * r_depth +
                            ev.PieceValue[@intFromEnum(self.b.pieceType(move.to))];
                        if (futil_val <= alpha) continue;
                    }

                    // SEE pruning (captures)
                    if (!see.see(self.b, move, &gen, SEE_CUTS[1][@intCast(depth)]))
                        continue;
                } else {
                    if (r_depth < 16 and !self.stack[ply].in_check) {
                        const futil_val = futility + 4 * ev.CentiPawn * r_depth;
                        if (futil_val <= alpha) {
                            if (!isMate(best_score) and
                                !isWin(futil_val) and
                                best_score < futil_val)
                                best_score = futil_val;
                            continue;
                        }
                    }

                    // SEE pruning (quiets)
                    if (!see.see(self.b, move, &gen, SEE_CUTS[0][@intCast(r_depth)]))
                        continue;
                }
            }

            // Extensions
            var E: i12 = 0;
            if (!root and self.stack[ply].excluded == null) {
                // Singular extension and multi cut
                if (self.stack[ply].stage == .TT and
                    depth >= 6 + @as(i12, @intCast(@intFromBool(tte_pv))) and
                    tte_depth >= depth - 3 and
                    tte.reader.?.val.typ != .Upper and
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
                        cutnode,
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
            self.nnw.apply(self.b, move, undo);

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

            self.nnw.remove();
            self.b.remove(move, undo);

            // Root stuff
            if (root) {
                var rm: *po.RootMove = &self.thread.root_moves.items[0];
                for (0..self.thread.root_moves.items.len) |i| {
                    if (self.thread.root_moves.items[i].move.equals(move)) {
                        rm = &self.thread.root_moves.items[i];
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
            best_score = if (self.stack[ply].in_check) mateVal(ply) else drawVal();
        }

        // if (!isMate(best_score) and best_score > beta)
        //     best_score = @divTrunc(best_score + beta, 2);

        // Update Stats
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
        if (move_counter > 0 and
            !self.stack[ply].in_check and
            (best_move == null or !self.b.isCapture(best_move.?)) and
            !(best_score >= beta and best_score <= static) and
            !(best_move == null and best_score >= static))
            self.corrections.update(self.b, prev, move, depth, best_score, static);

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

    pub fn aspiration(self: *Searcher, depth: i12) !i32 {
        const avg_sq: i32 = @intCast(@abs(self.thread.best_root.avg_score_sq));
        var delta = @divFloor(ev.CentiPawn, 8) + @divFloor(avg_sq, ev.CentiPawn * 6);
        var alpha = self.thread.best_root.avg_score - delta;
        var beta = self.thread.best_root.avg_score + delta;

        while (true) {
            const score = try self.search(alpha, beta, depth, false);

            if (score <= alpha) {
                beta = @divTrunc(alpha + beta, 2);
                alpha = score - delta;
                if (alpha < -MateVal) alpha = -MateVal;
            } else if (score >= beta) {
                beta = score + delta;
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
            if (se_depth <= 0) break;

            // For the start, we just do a normal search
            if (score == -MateVal) {
                score = self.search(-MateVal, MateVal, se_depth, false) catch |err| {
                    switch (err) {
                        error.NoTime => break,
                        else => return err,
                    }
                };
            } else {
                score = self.aspiration(se_depth) catch |err| {
                    switch (err) {
                        error.NoTime => break,
                        else => return err,
                    }
                };
            }
            self.thread.sortRootMoves();
            depth += self.thread.iter;

            self.clearStack();
        }
    }
};
