const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");

pub const TT_Type = enum(u2) {
    Lower,
    Upper,
    Exact,
};

pub const TT_Entry = struct {
    check: u64,
    hash: u64,
    // All of these combined are 64 bits
    score: i32,
    move: tp.Move,
    depth: u8, //This is a u8 to allow for more room for insert_time
    typ: TT_Type,
    insert_time: u6,

    pub inline fn getCheck(self: *const TT_Entry) u64 {
        return self.hash ^ @as(*u64, @ptrCast(@constCast(@alignCast(&self.score)))).*;
    }

    pub inline fn valid(self: *const TT_Entry) bool {
        return self.check == self.getCheck();
    }

    pub inline fn usable(self: *const TT_Entry, alpha: i32, beta: i32) bool {
        return self.typ == .Exact or
            (self.typ == .Lower and self.score >= beta) or
            (self.typ == .Upper and self.score < alpha);
    }
};

const TT_Size = (1 << 20) / @sizeOf(TT_Entry);
pub var TT = std.mem.zeroes([TT_Size]TT_Entry);

pub const TT_Result_Type = enum(u2) { Fine, Corrupt_Err, Override_Err };
pub const TT_Result = packed struct {
    entry: *TT_Entry,
    typ: TT_Result_Type,
};

pub inline fn probe(b: *const bo.Board) TT_Result {
    const res = &TT[b.hash[b.hash_in] % TT_Size];

    if (!res.valid()) return .{ .entry = res, .typ = .Corrupt_Err };
    if (b.hash[b.hash_in] != res.hash) return .{ .entry = res, .typ = .Override_Err };

    return .{ .entry = res, .typ = .Fine };
}

inline fn put(
    b: *const bo.Board,
    score: i32,
    move: tp.Move,
    depth: i12,
    alpha: i32,
    beta: i32,
    insert_time: u6,
) void {
    const index = b.hash[b.hash_in] % TT_Size;
    TT[index] = .{
        .check = 0,
        .hash = b.hash[b.hash_in],
        .score = score,
        .move = move,
        .depth = @intCast(depth), //This is a u8 to allow for more room for insert_time
        .typ = if (score >= beta) .Lower else if (score < alpha) .Upper else .Exact,
        .insert_time = insert_time,
    };

    TT[index].check = TT[index].getCheck();
}

pub inline fn store(
    b: *const bo.Board,
    score: i32,
    depth: i12,
    alpha: i32,
    beta: i32,
    bestMove: tp.Move,
    insert_time: u6,
    tte: TT_Result,
) void {
    if (tte.typ == .Corrupt_Err) put(b, score, bestMove, depth, alpha, beta, insert_time);

    const time_diff = insert_time - @as(u12, @intCast(tte.entry.insert_time));

    const tte_val = @as(i12, @intCast(tte.entry.depth)) - @as(i12, @intCast(time_diff)) * 8;
    const our_val = @as(i12, depth);

    if (our_val > tte_val) put(b, score, bestMove, depth, alpha, beta, insert_time);
}
