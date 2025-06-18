const std = @import("std");
const tp = @import("types.zig");
const mv = @import("movegen.zig");
const se = @import("search.zig");
const tt = @import("tt.zig");
const ev = @import("eval.zig");
const hi = @import("history.zig");
const see = @import("see.zig");

pub const Stage = enum {
    // Hash move
    TT,

    // Good Captures
    GenCaptures,
    GoodCaptures,

    // Killer move
    Killer,

    // Good Quiets
    GenQuiets,
    GoodQuiets,

    // Bad Captures
    BadCaptures,

    // Bad Quiets
    BadQuiets,

    // ProbCut
    ProbCutTT,
    GenProbCut,
    ProbCut,

    // For quiet search
    QuietSearchTT,
    GenQuietSearch,
    QuietSearch,
};

pub const Picker = struct {
    search: *const se.Searcher,
    gen: *const mv.Maker,
    list: std.ArrayList(tp.Move),
    score_list: std.ArrayList(i32),
    start: usize,
    pawn_attacked: tp.BitBoard,

    stage: Stage,
    ret_stage: Stage,
    tt: ?tp.Move,
    searched_tt: bool,
    killer: ?tp.Move,
    searched_killer: bool,
    skip_quiets: bool,
    current_val: ?i32,
    threshold: ?i32,

    pub fn init(
        stage: Stage,
        search: *const se.Searcher,
        gen: *const mv.Maker,
        hash_move: ?tp.Move,
        pawn_attacked: tp.BitBoard,
        threshold: ?i32,
    ) Picker {
        const ply = search.b.hash_in - search.start_ply;

        return .{
            .search = search,
            .gen = gen,
            .list = .init(search.alloc),
            .score_list = .init(search.alloc),
            .start = 0,
            .pawn_attacked = pawn_attacked,
            .stage = stage,
            .ret_stage = stage,
            .tt = hash_move,
            .searched_tt = false,
            .killer = if (ply >= 1) search.stack[ply - 1].killer else null,
            .searched_killer = false,
            .skip_quiets = false,
            .current_val = null,
            .threshold = threshold,
        };
    }

    pub inline fn deinit(self: *Picker) void {
        self.list.deinit();
        self.score_list.deinit();
    }

    fn goodCapturesFilter(pick: *const Picker, move: tp.Move, score: i32) bool {
        return pick.search.b.isNoisy(move) and
            see.see(
                pick.search.b,
                move,
                pick.gen,
                @divFloor(-score, hi.CentiHist * 6),
            );
    }
    fn goodQuietsFilter(pick: *const Picker, move: tp.Move, score: i32) bool {
        return !pick.search.b.isNoisy(move) and score > 0;
    }
    fn badCapturesFilter(pick: *const Picker, move: tp.Move, _: i32) bool {
        return pick.search.b.isNoisy(move);
    }
    fn badQuietsFilter(pick: *const Picker, move: tp.Move, _: i32) bool {
        return !pick.search.b.isNoisy(move);
    }
    fn probCutFilter(pick: *const Picker, move: tp.Move, _: i32) bool {
        return see.see(
            pick.search.b,
            move,
            pick.gen,
            pick.threshold.?,
        );
    }
    fn noFilter(_: *const Picker, _: tp.Move, _: i32) bool {
        return true;
    }

    inline fn pickMove(
        self: *Picker,
        comptime filter: fn (*const Picker, tp.Move, i32) bool,
    ) ?usize {
        if (self.list.items.len == self.start) return null;

        var best = self.start;
        var i = self.start;
        while (i < self.list.items.len) : (i += 1) {
            if (self.searched_tt and self.tt.?.equals(self.list.items[i])) {
                _ = self.score_list.swapRemove(i);
                _ = self.list.swapRemove(i);
                if (i >= self.list.items.len) break;
            }
            if (self.searched_killer and self.killer.?.equals(self.list.items[i])) {
                _ = self.score_list.swapRemove(i);
                _ = self.list.swapRemove(i);
                if (i >= self.list.items.len) break;
            }

            if (!filter(self, self.list.items[i], self.score_list.items[i])) {
                std.mem.swap(tp.Move, &self.list.items[i], &self.list.items[self.start]);
                std.mem.swap(
                    i32,
                    &self.score_list.items[i],
                    &self.score_list.items[self.start],
                );
                if (best == self.start) best = i;
                self.start += 1;
            } else if (self.score_list.items[i] > self.score_list.items[best]) best = i;
        }

        return if (self.list.items.len == self.start) null else best;
    }

    inline fn nextStage(self: *Picker, stage: Stage, comptime reset: bool) void {
        self.stage = stage;
        if (reset) self.start = 0;
    }

    inline fn scoreMoves(self: *Picker, start: usize) !void {
        const ply = self.search.b.hash_in - self.search.start_ply;

        try self.score_list.ensureTotalCapacity(self.list.items.len);
        for (start..self.list.items.len) |i| {
            const move = self.list.items[i];
            var score: i32 = 0;

            if (self.gen.dat.our_king != move.from) {
                // Penalty for moving to a square attacked by a pawn
                if (self.pawn_attacked.check(move.to)) {
                    score -= ev.PieceValue[@intFromEnum(self.search.b.pieceType(move.from))];
                    score += @divExact(ev.PawnBase, 2);
                }

                // Bonus for evading a pawn attack
                if (self.pawn_attacked.check(move.from) and
                    !self.pawn_attacked.check(move.to))
                {
                    score += ev.PieceValue[@intFromEnum(self.search.b.pieceType(move.from))];
                    score -= @divExact(ev.PawnBase, 2);
                }

                if (self.search.b.isCapture(move))
                    score += ev.PieceValue[@intFromEnum(self.search.b.pieceType(move.to))];
            } else score -= ev.CentiPawn * 5;

            // We convert the previous boni/mali to history vals
            score *= hi.CentiHist;

            // History boni
            score += self.search.stats.get(
                self.search.b,
                if (ply == 0) null else self.search.stack[ply - 1].move,
                move,
            );

            self.score_list.appendAssumeCapacity(score);
        }
    }

    pub fn nextMove(self: *Picker) !?tp.Move {
        @setEvalBranchQuota(2048);

        switch (self.stage) {
            .TT => {
                self.nextStage(.GenCaptures, false);
                if (self.tt != null and self.gen.isLegal(self.tt.?)) {
                    self.current_val = null;
                    if (!self.searched_killer or !self.killer.?.equals(self.tt.?)) {
                        self.searched_tt = true;
                        self.ret_stage = .TT;
                        return self.tt;
                    }
                }
            },
            .GenCaptures => {
                const start = self.list.items.len;
                try self.gen.gen(&self.list, .Noisy);
                try self.scoreMoves(start);

                self.nextStage(.GoodCaptures, false);
            },
            .GoodCaptures => {
                const picked = self.pickMove(goodCapturesFilter);
                if (picked == null)
                    self.nextStage(.Killer, false)
                else {
                    self.current_val = self.score_list.swapRemove(picked.?);
                    self.ret_stage = .GoodCaptures;
                    return self.list.swapRemove(picked.?);
                }
            },
            .Killer => {
                self.nextStage(.GenQuiets, false);
                if (self.killer != null and self.gen.isLegal(self.killer.?)) {
                    self.current_val = null;
                    if (!self.searched_tt or !self.tt.?.equals(self.killer.?)) {
                        self.searched_killer = true;
                        self.ret_stage = .Killer;
                        return self.killer;
                    }
                }
            },
            .GenQuiets => {
                if (self.skip_quiets)
                    self.nextStage(.GoodQuiets, false)
                else {
                    const start = self.list.items.len;
                    try self.gen.gen(&self.list, .Quiet);
                    try self.gen.gen(&self.list, .Castle);

                    try self.scoreMoves(start);

                    self.nextStage(.GoodQuiets, false);
                }
            },
            .GoodQuiets => {
                if (self.skip_quiets)
                    self.nextStage(.BadCaptures, true)
                else {
                    const picked = self.pickMove(goodQuietsFilter);
                    if (picked == null)
                        self.nextStage(.BadCaptures, true)
                    else {
                        self.current_val = self.score_list.swapRemove(picked.?);
                        self.ret_stage = .GoodQuiets;
                        return self.list.swapRemove(picked.?);
                    }
                }
            },
            .BadCaptures => {
                const picked = self.pickMove(badCapturesFilter);
                if (picked == null)
                    self.nextStage(.BadQuiets, true)
                else {
                    self.current_val = self.score_list.swapRemove(picked.?);
                    self.ret_stage = .BadCaptures;
                    return self.list.swapRemove(picked.?);
                }
            },
            .BadQuiets => {
                if (self.skip_quiets) return null;

                const picked = self.pickMove(badQuietsFilter);
                if (picked == null)
                    return null
                else {
                    self.current_val = self.score_list.swapRemove(picked.?);
                    self.ret_stage = .BadQuiets;
                    return self.list.swapRemove(picked.?);
                }
            },
            .ProbCutTT => {
                self.nextStage(.GenProbCut, false);
                if (self.tt != null and self.gen.isLegal(self.tt.?)) {
                    self.current_val = null;
                    if (!self.searched_killer or !self.killer.?.equals(self.tt.?)) {
                        self.searched_tt = true;
                        self.ret_stage = .ProbCutTT;
                        return self.tt;
                    }
                }
            },
            .GenProbCut => {
                const start = self.list.items.len;
                try self.gen.gen(&self.list, .Noisy);
                try self.scoreMoves(start);

                self.nextStage(.ProbCut, false);
            },
            .ProbCut => {
                const picked = self.pickMove(probCutFilter);
                if (picked == null)
                    return null
                else {
                    self.current_val = self.score_list.swapRemove(picked.?);
                    self.ret_stage = .ProbCut;
                    return self.list.swapRemove(picked.?);
                }
            },
            .QuietSearchTT => {
                self.nextStage(.GenQuietSearch, false);
                if (self.tt != null and self.gen.isLegal(self.tt.?)) {
                    self.current_val = null;
                    if (!self.searched_killer or !self.killer.?.equals(self.tt.?)) {
                        self.searched_tt = true;
                        self.ret_stage = .QuietSearchTT;
                        return self.tt;
                    }
                }
            },
            .GenQuietSearch => {
                const start = self.list.items.len;
                if (self.gen.checks > 0)
                    try self.gen.gen(&self.list, .Either)
                else
                    try self.gen.gen(&self.list, .Noisy);

                try self.scoreMoves(start);

                self.nextStage(.QuietSearch, false);
            },
            .QuietSearch => {
                const picked = self.pickMove(noFilter);
                if (picked == null)
                    return null
                else {
                    self.current_val = self.score_list.swapRemove(picked.?);
                    self.ret_stage = .QuietSearch;
                    return self.list.swapRemove(picked.?);
                }
            },
        }

        return self.nextMove();
    }
};
