const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");

// This is adressed by [side][from][to] (All moves)
const ButterflyHistory = [2][64][64]i32;
const ButterflyMax = (1 << 12);

// This is adressed by [side][piece][piece_to][to] (Captures)
const CaptureHistory = [2][6][5][64]i32;
const CaptureMax = (1 << 14);

// This is adressed by [side][piece][to] (Quiets)
const MoveHistory = [2][6][64]i32;
const MoveMax = (1 << 15);

// This is adressed by [side][piece][to][followup_piece][followup_to] (Quiets)
const FollowupMax = MoveMax;
const FollowupHistory = [2][6][64][6][64]i32;

pub const CentiHist = 64;

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

    inline fn gravity(max: comptime_int, bonus: i32, prev: i32) i32 {
        const clamp = std.math.clamp(bonus, -max, max);
        return clamp - @divFloor(prev * @as(i32, @intCast(@abs(clamp))), max);
    }

    inline fn butterfly_in(self: *const Stats, b: *const bo.Board, move: tp.Move) *i32 {
        const side = @intFromEnum(b.side);
        const from = @intFromEnum(move.from);
        const to = @intFromEnum(move.to);

        return @constCast(&self.butterfly[side][from][to]);
    }

    inline fn capture_in(self: *const Stats, b: *const bo.Board, move: tp.Move) *i32 {
        const side = @intFromEnum(b.side);
        const piece = @intFromEnum(b.pieceType(move.from));
        const piece_to = @intFromEnum(b.pieceType(move.to));
        const to = @intFromEnum(move.to);

        return @constCast(&self.capture[side][piece][piece_to][to]);
    }

    inline fn move_in(self: *const Stats, b: *const bo.Board, move: tp.Move) *i32 {
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
    ) *i32 {
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
    ) i32 {
        const quiet = b.isQuiet(move);
        var ret: i32 = self.butterfly_in(b, move).*;

        if (!quiet) {
            ret += self.capture_in(b, move).*;
        } else {
            ret += self.move_in(b, move).*;
            if (prev) |p| ret += self.followup_in(b, p, move).*;
        }

        return ret;
    }

    pub inline fn update(
        self: *Stats,
        b: *const bo.Board,
        list: *const std.ArrayList(tp.Move),
        prev: ?tp.Move,
        depth: i12,
        move_count: u8,
        diff: i32,
    ) void {
        const big_depth: i32 = @intCast(depth);
        const depth_sq = big_depth * big_depth;

        // Bonuses
        var bonus = depth_sq * (2 - @as(i3, @intCast(@intFromBool(move_count <= 3))));
        bonus += @min(diff, 49);

        const main = list.items[list.items.len - 1];
        const quiet = b.isQuiet(main);
        const butterfly = self.butterfly_in(b, main);

        butterfly.* += gravity(ButterflyMax, bonus, butterfly.*);
        if (!quiet) {
            const capture = self.capture_in(b, main);
            capture.* += gravity(ButterflyMax, bonus, capture.*);
        } else {
            const move = self.move_in(b, main);
            move.* += gravity(MoveMax, bonus, move.*);

            if (prev) |p| {
                const followup = self.followup_in(b, p, main);
                followup.* += gravity(FollowupMax, bonus, followup.*);
            }
        }

        const quiet_factor: i3 = @intCast(@intFromBool(quiet));
        // Maluses
        for (0..(list.items.len - 1)) |i| {
            const m = list.items[i];

            const this_quiet = b.isQuiet(m);

            const this_factor: i3 = @intCast(@intFromBool(this_quiet));
            const malus = big_depth * (1 + quiet_factor - this_factor);

            const this_butterfly = self.butterfly_in(b, m);
            this_butterfly.* += gravity(ButterflyMax, -malus, this_butterfly.*);
            if (!this_quiet) {
                const capture = self.capture_in(b, main);
                capture.* += gravity(ButterflyMax, -malus, capture.*);
            } else {
                const move = self.move_in(b, main);
                move.* += gravity(MoveMax, -malus, move.*);

                if (prev) |p| {
                    const followup = self.followup_in(b, p, m);
                    followup.* += gravity(FollowupMax, -malus, followup.*);
                }
            }
        }
    }
};
