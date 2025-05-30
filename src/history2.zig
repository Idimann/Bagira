const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const se2 = @import("search2.zig");

const Value = packed struct {
    p: se2.Searcher.Prob,
    n: u32,
};
// This is adressed by [side][from][to] (All moves)
const ButterflyHistory = [2][64][64]Value;

// This is adressed by [side][piece][piece_to][to] (Captures)
const CaptureHistory = [2][6][5][64]Value;

// This is adressed by [side][piece][to] (Quiets)
const MoveHistory = [2][6][64]Value;

// This is adressed by [side][piece][to][followup_piece][followup_to] (Quiets)
const FollowupHistory = [2][6][64][6][64]Value;

pub const Stats = struct {
    butterfly: ButterflyHistory,
    capture: CaptureHistory,
    move: MoveHistory,
    followup: FollowupHistory,

    pub inline fn init() Stats {
        return .{
            .butterfly = std.mem.zeroes(ButterflyHistory),
            .capture = std.mem.zeroes(CaptureHistory),
            .move = std.mem.zeroes(MoveHistory),
            .followup = std.mem.zeroes(FollowupHistory),
        };
    }

    inline fn append(val: se2.Searcher.Prob, prev: *Value) void {
        prev.p += val;
        prev.n += 1;
    }

    inline fn add(val: Value, val2: Value) Value {
        return .{
            .p = val.p + val2.p,
            .n = val.n + val2.n,
        };
    }

    inline fn butterfly_in(self: *const Stats, b: *const bo.Board, move: tp.Move) *Value {
        const side = @intFromEnum(b.side);
        const from = @intFromEnum(move.from);
        const to = @intFromEnum(move.to);

        return @constCast(&self.butterfly[side][from][to]);
    }

    inline fn capture_in(self: *const Stats, b: *const bo.Board, move: tp.Move) *Value {
        const side = @intFromEnum(b.side);
        const piece = @intFromEnum(b.pieceType(move.from));
        const piece_to = @intFromEnum(b.pieceType(move.to));
        const to = @intFromEnum(move.to);

        return @constCast(&self.capture[side][piece][piece_to][to]);
    }

    inline fn move_in(self: *const Stats, b: *const bo.Board, move: tp.Move) *Value {
        const side = @intFromEnum(b.side);
        const piece = @intFromEnum(b.pieceType(move.from));
        const to = @intFromEnum(move.to);

        return @constCast(&self.move[side][piece][to]);
    }

    inline fn followup_in(
        self: *const Stats,
        b: *const bo.Board,
        prev: tp.Move,
        move: tp.Move,
    ) *Value {
        const side = @intFromEnum(b.side);
        const piece = @intFromEnum(b.pieceType(prev.from));
        const to = @intFromEnum(prev.to);
        const f_piece = @intFromEnum(b.pieceType(move.from));
        const f_to = @intFromEnum(move.to);

        return @constCast(&self.followup[side][piece][to][f_piece][f_to]);
    }

    pub inline fn get(
        self: *const Stats,
        b: *const bo.Board,
        prev: ?tp.Move,
        move: tp.Move,
    ) se2.Searcher.Prob {
        const quiet = b.isQuiet(move);
        var ret = self.butterfly_in(b, move).*;

        if (!quiet) {
            ret = add(ret, self.capture_in(b, move).*);
        } else {
            ret = add(ret, self.move_in(b, move).*);
            if (prev) |p| ret = add(ret, self.followup_in(b, p, move).*);
        }

        return ret.p / @as(se2.Searcher.Prob, @floatFromInt(ret.n));
    }

    pub inline fn update(
        self: *Stats,
        b: *const bo.Board,
        main: tp.Move,
        prev: ?tp.Move,
        res: se2.Searcher.Prob,
    ) void {
        const butterfly = self.butterfly_in(b, main);
        append(res, butterfly);

        const quiet = b.isQuiet(main);
        if (!quiet) {
            const capture = self.capture_in(b, main);
            append(res, capture);
        } else {
            const move = self.move_in(b, main);
            append(res, move);

            if (prev) |p| {
                const followup = self.followup_in(b, p, main);
                append(res, followup);
            }
        }
    }
};
