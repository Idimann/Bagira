const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const se = @import("search.zig");

const HashMask = 0xFFFFFFFF;
pub const TT_Entry = struct {
    check: u64 = 0,
    // All of these combined are 128 bits
    val: packed struct {
        hash: u32 = 0,
        score: se.Searcher.BoundProb = .{ .p = 0.0, .bound = 1 },
        move: tp.Move = std.mem.zeroes(tp.Move),
        insert_time: u17 = 0,
    } = .{},

    pub inline fn getCheck(self: *const TT_Entry) u64 {
        const arr: [*]const u64 = @ptrCast(&self.val);

        return arr[0] ^ arr[1];
    }

    pub inline fn valid(self: *const TT_Entry) bool {
        return self.check == self.getCheck();
    }

    pub inline fn usable(self: *const TT_Entry, bound: se.Searcher.Prob) bool {
        return self.val.score.bound < bound;
    }
};

const TT_Size = (25 << 20) / @sizeOf(TT_Entry);
pub var TT: [TT_Size]TT_Entry = .{.{}} ** TT_Size;

pub const TT_Result = struct {
    reader: ?TT_Entry,
    usable: bool,
};

pub inline fn probe(b: *const bo.Board) TT_Result {
    const read = TT[b.hash[b.hash_in] % TT_Size];

    if (!read.valid()) return .{
        .reader = null,
        .usable = false,
    };
    if (@as(u32, @intCast(b.hash[b.hash_in] & HashMask)) != read.val.hash) return .{
        .reader = read,
        .usable = false,
    };

    return .{
        .reader = read,
        .usable = true,
    };
}

inline fn put(
    b: *const bo.Board,
    score: se.Searcher.BoundProb,
    move: tp.Move,
    insert_time: u17,
) void {
    const index = b.hash[b.hash_in] % TT_Size;
    var entry = TT_Entry{
        .check = 0,
        .val = .{
            .hash = @intCast(b.hash[b.hash_in] & HashMask),
            .score = score,
            .move = move,
            .insert_time = insert_time,
        },
    };
    entry.check = entry.getCheck();
    TT[index] = entry;
}

pub inline fn store(
    b: *const bo.Board,
    score: se.Searcher.BoundProb,
    move: tp.Move,
    insert_time: u17,
    tte: TT_Result,
) void {
    if (tte.reader == null) {
        put(
            b,
            score,
            move,
            insert_time,
        );
        return;
    }

    const tte_time = tte.reader.?.val.insert_time;
    const time_diff = if (insert_time < tte_time)
        64
    else
        insert_time - tte_time;

    const tte_val = (1 - tte.reader.?.val.score.bound) -
        @as(se.Searcher.Prob, @floatFromInt(time_diff)) * 0.25;
    const our_val = 1 - score.bound;

    if (our_val > tte_val) put(
        b,
        score,
        move,
        insert_time,
    );
}
