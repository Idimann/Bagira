const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const mv = @import("movegen.zig");
const po = @import("pool.zig");
const pi = @import("movepick.zig");
const hi = @import("history.zig");
const tt = @import("tt.zig");

// We represent probabilities as u12
pub const Searcher = struct {
    alloc: std.mem.Allocator,
    b: *bo.Board,
    start_ply: u12,
    pv: ?tp.Move,
    stats: hi.Stats,
    thread: *po.Thread,

    pub fn init(thread: *po.Thread, alloc: std.mem.Allocator) Searcher {
        return .{
            .alloc = alloc,
            .b = &thread.board,
            .start_ply = thread.board.hash_in,
            .pv = null,
            .stats = hi.Stats.init(),
            .thread = thread,
        };
    }

    pub const Prob = f32;
    pub const BoundProb = packed struct {
        p: Prob,
        bound: Prob,
    };

    const DrawVal = BoundProb{ .p = 0.5, .bound = 0 };
    const LossVal = BoundProb{ .p = 0.0, .bound = 0 };

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

    pub fn search(self: *Searcher, bound: Prob, until_prob: Prob) !BoundProb {
        if (self.thread.stopped) return error.NoTime;

        // Check for basic draws
        if (self.historyDraw() or self.materialDraw()) return DrawVal;
        const ply: u24 = @intCast(self.b.hash_in - self.start_ply);

        // TT Probe
        const tte = tt.probe(self.b);
        const tte_fine = tte.reader != null and tte.usable;

        // Trust the tt entry if it's usable
        if (ply != 0 and tte_fine and tte.reader.?.usable(bound))
            return tte.reader.?.val.score;

        const gen = mv.Maker.init(self.b);

        var until: Prob = 1.0;
        var this_prob: Prob = 0.0;
        var best_score: Prob = 0.0;
        const ply_sq: Prob = @floatFromInt((1 + ply) * (1 + ply));

        var pick = pi.Picker.init(.TT, self, &gen, tte);
        defer pick.deinit();

        var best_move: ?tp.Move = null;
        var best_cut: Prob = 0.0;
        while (try pick.nextMove()) |move| {
            const undo = self.b.apply(move);
            const result = try self.search(bound, until * until_prob);
            self.b.remove(move, undo);

            const score = 1 - result.p;
            if (result.bound > best_cut) best_cut = result.bound;

            self.stats.update(self.b, move, null, score);

            const move_prob = score * until;

            // We assume the player will always see the best move first
            if (score > best_score) {
                best_move = move;
                this_prob *= 1 - score;
                this_prob += score * score;
                best_score = score;
            } else this_prob += score * move_prob;
            until *= 1 - score;

            if (best_move == null) best_move = move;

            const cut = move_prob * until_prob + 1 / ply_sq;
            if (cut < bound) {
                if (cut > best_cut) best_cut = cut;
                this_prob += (1 - score) * move_prob;
                break;
            }
        }

        // Check or stalemate
        if (best_move == null) return if (gen.checks > 0) LossVal else DrawVal;

        // Update pv
        if (ply == 0) self.pv = best_move;

        const ret = BoundProb{
            .p = this_prob,
            .bound = best_cut,
        };

        // TT insert
        tt.store(
            self.b,
            ret,
            best_move.?,
            @intCast(self.start_ply),
            tte,
        );

        return ret;
    }

    pub fn deepening(self: *Searcher) !void {
        var bound = self.thread.iter;

        while (true) {
            self.thread.res.move = self.pv;
            self.thread.res.bp = self.search(bound, 1) catch |err| {
                switch (err) {
                    error.NoTime => break,
                    else => return err,
                }
            };

            bound = @min(self.thread.res.bp.bound, bound * self.thread.iter);
        }
    }
};
