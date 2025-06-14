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

    // Bad Captures
    BadCaptures,

    // Quiets
    GenQuiets,
    Quiets,

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

    inline fn reset(self: *Picker) void {
        self.list.clearRetainingCapacity();
        self.score_list.clearRetainingCapacity();
    }

    inline fn pickMove(self: *Picker) ?usize {
        if (self.list.items.len == 0) return null;

        var best: usize = 0;
        for (0..self.list.items.len) |i| {
            if (self.searched_tt and self.tt.?.equals(self.list.items[i])) {
                _ = self.score_list.swapRemove(i);
                _ = self.list.swapRemove(i);

                if (self.list.items.len == 0) return null;
            }
            if (self.searched_killer and self.killer.?.equals(self.list.items[i])) {
                _ = self.score_list.swapRemove(i);
                _ = self.list.swapRemove(i);

                if (self.list.items.len == 0) return null;
            }

            if (self.score_list.items[i] > self.score_list.items[best]) best = i;
        }

        return best;
    }

    inline fn scoreMoves(self: *Picker) !void {
        const ply = self.search.b.hash_in - self.search.start_ply;

        try self.score_list.ensureTotalCapacity(self.list.items.len);
        for (0..self.list.items.len) |i| {
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

                if (!self.search.b.isQuiet(move))
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
                self.stage = .GenCaptures;
                if (self.tt != null and self.gen.isLegal(self.tt.?)) {
                    self.current_val = null;
                    if (!self.searched_killer or !self.killer.?.equals(self.tt.?)) {
                        self.ret_stage = .TT;
                        return self.tt;
                    }
                }
            },
            .GenCaptures => {
                self.reset();
                try self.gen.gen(&self.list, .Capture);
                try self.scoreMoves();

                self.stage = .GoodCaptures;
            },
            .GoodCaptures => {
                const picked = self.pickMove();
                if (picked == null or !see.see(
                    self.search.b,
                    self.list.items[picked.?],
                    self.gen,
                    @divFloor(-self.score_list.items[picked.?], hi.CentiHist),
                ))
                    self.stage = .Killer
                else {
                    self.current_val = self.score_list.swapRemove(picked.?);
                    self.ret_stage = .GoodCaptures;
                    return self.list.swapRemove(picked.?);
                }
            },
            .Killer => {
                self.stage = .BadCaptures;
                if (self.killer != null and self.gen.isLegal(self.killer.?)) {
                    self.current_val = null;
                    if (!self.searched_tt or !self.tt.?.equals(self.killer.?)) {
                        self.ret_stage = .Killer;
                        return self.killer;
                    }
                }
            },
            .BadCaptures => {
                const picked = self.pickMove();
                if (picked == null)
                    self.stage = .GenQuiets
                else {
                    self.current_val = self.score_list.swapRemove(picked.?);
                    self.ret_stage = .BadCaptures;
                    return self.list.swapRemove(picked.?);
                }
            },
            .GenQuiets => {
                if (self.skip_quiets) return null;

                self.reset();
                try self.gen.gen(&self.list, .Quiet);
                try self.gen.gen(&self.list, .Castle);

                try self.scoreMoves();

                self.stage = .Quiets;
            },
            .Quiets => {
                if (self.skip_quiets) return null;

                const picked = self.pickMove();
                if (picked == null)
                    return null
                else {
                    self.current_val = self.score_list.swapRemove(picked.?);
                    self.ret_stage = .Quiets;
                    return self.list.swapRemove(picked.?);
                }
            },
            .ProbCutTT => {
                self.stage = .GenProbCut;
                if (self.tt != null and self.gen.isLegal(self.tt.?)) {
                    self.current_val = null;
                    if (!self.searched_killer or !self.killer.?.equals(self.tt.?)) {
                        self.ret_stage = .ProbCutTT;
                        return self.tt;
                    }
                }
            },
            .GenProbCut => {
                self.reset();
                try self.gen.gen(&self.list, .Capture);
                try self.scoreMoves();

                self.stage = .ProbCut;
            },
            .ProbCut => {
                const picked = self.pickMove();
                if (picked == null or !see.see(
                    self.search.b,
                    self.list.items[picked.?],
                    self.gen,
                    self.threshold.?,
                ))
                    return null
                else {
                    self.current_val = self.score_list.swapRemove(picked.?);
                    self.ret_stage = .ProbCut;
                    return self.list.swapRemove(picked.?);
                }
            },
            .QuietSearchTT => {
                self.stage = .GenQuietSearch;
                if (self.tt != null and self.gen.isLegal(self.tt.?)) {
                    self.current_val = null;
                    if (!self.searched_killer or !self.killer.?.equals(self.tt.?)) {
                        self.ret_stage = .QuietSearchTT;
                        return self.tt;
                    }
                }
            },
            .GenQuietSearch => {
                self.reset();
                if (self.gen.checks > 0)
                    try self.gen.gen(&self.list, .Either)
                else
                    try self.gen.gen(&self.list, .Capture);

                try self.scoreMoves();

                self.stage = .QuietSearch;
            },
            .QuietSearch => {
                const picked = self.pickMove();
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
