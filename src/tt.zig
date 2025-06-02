const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");

pub const TT_Type = enum(u2) {
    Lower,
    Upper,
    Exact,
};

const HashMask = 0xFFFFFFFF;
pub const TT_Entry = struct {
    check: u64,
    // All of these combined are 64 bits
    val: packed struct {
        hash: u32,
        score: i32,
        eval: i32,
        move: tp.Move,
        depth: u8, //This is a u8 to allow for more room for insert_time
        typ: TT_Type,
        insert_time: u7,
    },

    pub inline fn getCheck(self: *const TT_Entry) u64 {
        const arr: [*]const u64 = @ptrCast(&self.val);

        return arr[0] ^ arr[1];
    }

    pub inline fn valid(self: *const TT_Entry) bool {
        return self.check == self.getCheck();
    }

    pub inline fn usable(self: *const TT_Entry, alpha: i32, beta: i32) bool {
        return self.val.typ == .Exact or
            (self.val.typ == .Lower and self.val.score >= beta) or
            (self.val.typ == .Upper and self.val.score < alpha);
    }
};

const TT_Size = (25 << 20) / @sizeOf(TT_Entry);
pub var TT = std.mem.zeroes([TT_Size]TT_Entry);

pub const TT_Result = struct {
    reader: ?TT_Entry,
    usable: bool,
};

pub fn clear() void {
    for (0..TT_Size) |i| TT[i] = std.mem.zeroes(TT_Entry);
}

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
    score: i32,
    eval: i32,
    move: ?tp.Move,
    depth: i12,
    lower_bound: i32,
    upper_bound: i32,
    insert_time: u7,
) void {
    const index = b.hash[b.hash_in] % TT_Size;
    var entry = TT_Entry{
        .check = 0,
        .val = .{
            .hash = @intCast(b.hash[b.hash_in] & HashMask),
            .score = std.math.clamp(score, lower_bound, upper_bound),
            .eval = eval,
            .move = if (move) |mov| mov else std.mem.zeroes(tp.Move),
            .depth = @intCast(@min(depth, std.math.maxInt(u8))),
            .typ = if (score >= upper_bound)
                .Lower
            else if (score <= lower_bound)
                .Upper
            else
                .Exact,
            .insert_time = insert_time,
        },
    };
    entry.check = entry.getCheck();
    TT[index] = entry;
}

pub inline fn store(
    b: *const bo.Board,
    score: i32,
    eval: i32,
    depth: i12,
    lower_bound: i32,
    upper_bound: i32,
    bestMove: ?tp.Move,
    insert_time: u12,
    tte: TT_Result,
) void {
    const real_insert_time = @as(i12, @intCast(insert_time % std.math.maxInt(u7)));

    if (tte.reader == null) {
        put(
            b,
            score,
            eval,
            bestMove,
            depth,
            lower_bound,
            upper_bound,
            @intCast(real_insert_time),
        );
        return;
    }

    const real_tte_time = @as(i12, @intCast(tte.reader.?.val.insert_time));
    const time_diff = if (real_insert_time < real_tte_time)
        64
    else
        real_insert_time - real_tte_time;

    const tte_val = @as(i12, @intCast(tte.reader.?.val.depth)) -
        @as(i12, @intCast(time_diff)) * 8;
    const our_val = @as(i12, depth);

    if (our_val > tte_val) put(
        b,
        score,
        eval,
        bestMove,
        depth,
        lower_bound,
        upper_bound,
        @intCast(real_insert_time),
    );
}
