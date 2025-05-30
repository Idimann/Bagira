const std = @import("std");
const tp = @import("types.zig");
const mv = @import("movegen.zig");
const se2 = @import("search2.zig");
const tt2 = @import("tt2.zig");

pub const Stage = enum {
    // Hash move
    TT,

    // Captures
    GenCaptures,
    Captures,

    // Quiets
    GenQuiets,
    Quiets,
};

pub const Picker = struct {
    search: *const se2.Searcher,
    gen: *const mv.Maker,
    list: std.ArrayList(tp.Move),
    score_list: std.ArrayList(se2.Searcher.Prob),
    tte: tt2.TT_Result,

    tt: ?tp.Move,
    stage: Stage,

    pub inline fn init(
        stage: Stage,
        search: *const se2.Searcher,
        gen: *const mv.Maker,
        tte: tt2.TT_Result,
    ) Picker {
        return .{
            .search = search,
            .gen = gen,
            .list = std.ArrayList(tp.Move).init(search.alloc),
            .score_list = std.ArrayList(se2.Searcher.Prob).init(search.alloc),
            .tt = null,
            .tte = tte,
            .stage = stage,
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

    pub fn nextMove(self: *Picker) !?tp.Move {
        switch (self.stage) {
            .TT => {
                self.stage = .GenCaptures;
                if (self.tte.reader != null and
                    self.tte.usable and
                    self.gen.isLegal(self.tte.reader.?.val.move))
                {
                    self.tt = self.tte.reader.?.val.move;
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
                        null,
                        self.list.items[i],
                    );
                    self.score_list.appendAssumeCapacity(score);
                }
                self.sortMoves();

                self.stage = .Captures;
            },
            .Captures => {
                if (self.list.items.len == 0)
                    self.stage = .GenQuiets
                else {
                    _ = self.score_list.pop();
                    const move = self.list.pop();

                    if (self.tt == null or !self.tt.?.equals(move)) return move;
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
                        null,
                        self.list.items[i],
                    );
                    self.score_list.appendAssumeCapacity(score);
                }
                self.sortMoves();

                self.stage = .Quiets;
            },
            .Quiets => {
                _ = self.score_list.popOrNull();
                const move = self.list.popOrNull();
                if (move == null) return move;

                if (self.tt == null or !self.tt.?.equals(move.?)) return move;
            },
        }

        return self.nextMove();
    }
};
