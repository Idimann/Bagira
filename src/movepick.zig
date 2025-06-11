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

    // For quiet search
    QuietTT,
    GenQuiet,
    Quiet,
};

pub const Picker = struct {
    search: *const se.Searcher,
    gen: *const mv.Maker,
    list: std.ArrayList(tp.Move),
    score_list: std.ArrayList(i32),
    pawn_attacked: ?tp.BitBoard,

    stage: Stage,
    ret_stage: Stage,
    skip_quiets: bool,
    tt: ?tp.Move,
    killer: ?tp.Move,
    current_hist: ?i32,

    pub inline fn init(
        stage: Stage,
        search: *const se.Searcher,
        gen: *const mv.Maker,
        hash_move: ?tp.Move,
    ) Picker {
        return .{
            .search = search,
            .gen = gen,
            .list = .init(search.alloc),
            .score_list = .init(search.alloc),
            .pawn_attacked = null,
            .stage = stage,
            .ret_stage = stage,
            .skip_quiets = false,
            .tt = hash_move,
            .killer = null,
            .current_hist = null,
        };
    }

    pub inline fn deinit(self: *Picker) void {
        self.list.deinit();
        self.score_list.deinit();
    }

    inline fn sortMoves(self: *Picker) void {
        if (self.list.items.len < 2) return;

        for (0..(self.list.items.len - 1)) |i| {
            for (0..(self.list.items.len - i - 1)) |j| {
                if (self.score_list.items[j] < self.score_list.items[j + 1]) {
                    const temp = self.list.items[j];
                    const score_temp = self.score_list.items[j];
                    self.list.items[j] = self.list.items[j + 1];
                    self.score_list.items[j] = self.score_list.items[j + 1];
                    self.list.items[j + 1] = temp;
                    self.score_list.items[j + 1] = score_temp;
                }
            }
        }
    }

    inline fn scoreMoves(self: *Picker) !void {
        const ply = self.search.b.hash_in - self.search.start_ply;
        if (self.pawn_attacked == null) self.pawn_attacked = self.gen.attackedPawn();

        self.score_list.items.len = 0;
        try self.score_list.ensureTotalCapacity(self.list.items.len);
        for (0..self.list.items.len) |i| {
            const move = self.list.items[i];
            var score: i32 = 0;

            // Penalty for moving to a square attacked by a pawn
            if (self.pawn_attacked.?.check(move.to)) {
                score -= ev.PieceValue[@intFromEnum(self.search.b.pieceType(move.from))];
                score += ev.PawnBase;
            }

            // We convert the previous boni/mali to history vals
            score *= hi.CentiHist;
            score = @divFloor(score, ev.CentiPawn);

            // History boni
            score += self.search.stats.get(
                self.search.b,
                if (ply == 0) null else self.search.stack[ply - 1].move,
                move,
            );

            self.score_list.appendAssumeCapacity(score);
        }

        self.sortMoves();
    }

    pub fn nextMove(self: *Picker) !?tp.Move {
        const ply = self.search.b.hash_in - self.search.start_ply;

        switch (self.stage) {
            .TT => {
                self.stage = .GenCaptures;
                if (self.tt != null and self.gen.isLegal(self.tt.?)) {
                    self.current_hist = null;
                    if (self.killer == null or
                        !self.killer.?.equals(self.tt.?))
                    {
                        self.ret_stage = .TT;
                        return self.tt;
                    }
                }
            },
            .GenCaptures => {
                try self.gen.gen(&self.list, .Capture);
                try self.scoreMoves();
                self.sortMoves();

                self.stage = .GoodCaptures;
            },
            .GoodCaptures => {
                if (self.list.items.len == 0)
                    self.stage = .Killer
                else {
                    const score = self.score_list.pop().?;
                    if (score < 0) self.stage = .Killer;
                    self.current_hist = score;
                    const move = self.list.pop().?;

                    if ((self.tt == null or !self.tt.?.equals(move)) and
                        (self.killer == null or !self.killer.?.equals(move)))
                    {
                        self.ret_stage = .GoodCaptures;
                        return move;
                    }
                }
            },
            .Killer => {
                self.stage = .BadCaptures;
                if (ply > 0) {
                    if (self.search.stack[ply - 1].killer) |move| {
                        if (self.gen.isLegal(move)) {
                            self.current_hist = null;
                            self.killer = move;
                            if (self.tt == null or !self.tt.?.equals(move)) {
                                self.ret_stage = .Killer;
                                return move;
                            }
                        }
                    }
                }
            },
            .BadCaptures => {
                if (self.list.items.len == 0)
                    self.stage = .GenQuiets
                else {
                    self.current_hist = self.score_list.pop().?;
                    const move = self.list.pop().?;

                    if ((self.tt == null or !self.tt.?.equals(move)) and
                        (self.killer == null or !self.killer.?.equals(move)))
                    {
                        self.ret_stage = .BadCaptures;
                        return move;
                    }
                }
            },
            .GenQuiets => {
                if (self.skip_quiets) return null;

                try self.gen.gen(&self.list, .Quiet);
                try self.gen.gen(&self.list, .Castle);

                try self.scoreMoves();
                self.sortMoves();

                self.stage = .Quiets;
            },
            .Quiets => {
                if (self.skip_quiets) return null;

                const move = self.list.pop();
                self.current_hist = self.score_list.pop();
                if (move == null) return move;

                if ((self.tt == null or !self.tt.?.equals(move.?)) and
                    (self.killer == null or !self.killer.?.equals(move.?)))
                {
                    self.ret_stage = .Quiets;
                    return move;
                }
            },
            .QuietTT => {
                self.stage = .GenQuiet;
                if (self.tt != null and self.gen.isLegal(self.tt.?)) {
                    self.current_hist = null;
                    if (self.killer == null or
                        !self.killer.?.equals(self.tt.?))
                    {
                        self.ret_stage = .QuietTT;
                        return self.tt;
                    }
                }
            },
            .GenQuiet => {
                if (self.skip_quiets) return null;

                if (self.gen.checks > 0)
                    try self.gen.gen(&self.list, .Either)
                else
                    try self.gen.gen(&self.list, .Capture);

                try self.scoreMoves();
                self.sortMoves();

                self.stage = .Quiet;
            },
            .Quiet => {
                if (self.skip_quiets) return null;

                const move = self.list.pop();
                self.current_hist = self.score_list.pop();
                if (move == null) return move;

                if ((self.tt == null or !self.tt.?.equals(move.?)) and
                    (self.killer == null or !self.killer.?.equals(move.?)))
                {
                    self.ret_stage = .Quiet;
                    return move;
                }
            },
        }

        return self.nextMove();
    }
};
