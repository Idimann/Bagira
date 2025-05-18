const std = @import("std");
const tp = @import("types.zig");
const mv = @import("movegen.zig");
const se = @import("search.zig");
const tt = @import("tt.zig");

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
    tte: tt.TT_Result,

    stage: Stage,
    tt: ?tp.Move,
    killer: ?tp.Move,
    current_hist: ?i32,

    pub inline fn init(
        stage: Stage,
        search: *const se.Searcher,
        gen: *const mv.Maker,
        tte: tt.TT_Result,
    ) Picker {
        return .{
            .search = search,
            .gen = gen,
            .list = std.ArrayList(tp.Move).init(search.alloc),
            .score_list = std.ArrayList(i32).init(search.alloc),
            .tte = tte,
            .stage = stage,
            .tt = null,
            .killer = null,
            .current_hist = null,
        };
    }

    pub inline fn deinit(self: *Picker) void {
        self.list.deinit();
        self.score_list.deinit();
    }

    inline fn sort_moves(self: *Picker) void {
        if (self.list.items.len < 2) return;

        for (0..(self.list.items.len / 2 + 1)) |i| {
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

        if (self.score_list.items[0] < self.score_list.items[1]) { //I know this is dumb
            const temp = self.list.items[0];
            const score_temp = self.score_list.items[0];
            self.list.items[0] = self.list.items[1];
            self.score_list.items[0] = self.score_list.items[0 + 1];
            self.list.items[1] = temp;
            self.score_list.items[1] = score_temp;
        }
    }

    pub fn nextMove(self: *Picker) !?tp.Move {
        const ply = self.search.b.hash_in - self.search.start_ply;

        switch (self.stage) {
            .TT => {
                self.stage = .GenCaptures;
                if (self.tte.reader != null and
                    self.tte.usable and
                    self.tte.reader.?.val.typ != .Upper and
                    self.gen.isLegal(self.tte.reader.?.val.move))
                {
                    self.current_hist = null;
                    self.tt = self.tte.reader.?.val.move;
                    if (self.killer == null or
                        !self.killer.?.equals(self.tte.reader.?.val.move))
                        return self.tte.reader.?.val.move;
                }
            },
            .GenCaptures => {
                try self.gen.gen(&self.list, .Capture);
                self.score_list.items.len = 0;
                try self.score_list.ensureTotalCapacity(self.list.items.len);

                for (0..self.list.items.len) |i| {
                    const score = self.search.stats.get(
                        self.search.b,
                        if (ply == 0) null else self.search.stack[ply - 1].move,
                        self.list.items[i],
                    );
                    self.score_list.appendAssumeCapacity(score);
                }
                self.sort_moves();

                self.stage = .GoodCaptures;
            },
            .GoodCaptures => {
                if (self.list.items.len == 0)
                    self.stage = .Killer
                else {
                    const score = self.score_list.pop();
                    if (score < 0) self.stage = .Killer;
                    self.current_hist = score;
                    const move = self.list.pop();

                    if ((self.tt == null or !self.tt.?.equals(move)) and
                        (self.killer == null or !self.killer.?.equals(move)))
                        return move;
                }
            },
            .Killer => {
                self.stage = .BadCaptures;
                if (ply > 0) {
                    if (self.search.stack[ply - 1].killer) |move| {
                        if (self.gen.isLegal(move)) {
                            self.current_hist = null;
                            self.killer = move;
                            if (self.tt == null or !self.tt.?.equals(move))
                                return move;
                        }
                    }
                }
            },
            .BadCaptures => {
                if (self.list.items.len == 0)
                    self.stage = .GenQuiets
                else {
                    self.current_hist = self.score_list.pop();
                    const move = self.list.pop();

                    if ((self.tt == null or !self.tt.?.equals(move)) and
                        (self.killer == null or !self.killer.?.equals(move)))
                        return move;
                }
            },
            .GenQuiets => {
                try self.gen.gen(&self.list, .Quiet);
                try self.gen.gen(&self.list, .Castle);
                self.score_list.items.len = 0;
                try self.score_list.ensureTotalCapacity(self.list.items.len);

                for (0..self.list.items.len) |i| {
                    const score = self.search.stats.get(
                        self.search.b,
                        if (ply == 0) null else self.search.stack[ply - 1].move,
                        self.list.items[i],
                    );
                    self.score_list.appendAssumeCapacity(score);
                }
                self.sort_moves();

                self.stage = .Quiets;
            },
            .Quiets => {
                const move = self.list.popOrNull();
                self.current_hist = self.score_list.popOrNull();
                if (move == null) return move;

                if ((self.tt == null or !self.tt.?.equals(move.?)) and
                    (self.killer == null or !self.killer.?.equals(move.?)))
                    return move;
            },
            .QuietTT => {
                self.stage = .GenQuiet;
                if (self.tte.reader != null and
                    self.tte.usable and
                    self.tte.reader.?.val.typ != .Upper and
                    self.gen.isLegal(self.tte.reader.?.val.move))
                {
                    self.current_hist = null;
                    self.tt = self.tte.reader.?.val.move;
                    if (self.killer == null or
                        !self.killer.?.equals(self.tte.reader.?.val.move))
                        return self.tte.reader.?.val.move;
                }
            },
            .GenQuiet => {
                if (self.gen.checks > 0)
                    try self.gen.gen(&self.list, .Either)
                else
                    try self.gen.gen(&self.list, .Capture);
                self.score_list.items.len = 0;
                try self.score_list.ensureTotalCapacity(self.list.items.len);

                for (0..self.list.items.len) |i| {
                    const score = self.search.stats.get(
                        self.search.b,
                        if (ply == 0) null else self.search.stack[ply - 1].move,
                        self.list.items[i],
                    );
                    self.score_list.appendAssumeCapacity(score);
                }
                self.sort_moves();

                self.stage = .Quiet;
            },
            .Quiet => {
                const move = self.list.popOrNull();
                self.current_hist = self.score_list.popOrNull();
                if (move == null) return move;

                if ((self.tt == null or !self.tt.?.equals(move.?)) and
                    (self.killer == null or !self.killer.?.equals(move.?)))
                    return move;
            },
        }

        return self.nextMove();
    }
};
