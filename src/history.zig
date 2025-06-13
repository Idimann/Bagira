const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const ev = @import("eval.zig");

// This is adressed by [side][from][to] (All moves)
const ButterflyHistory = [2][64][64]i32;
const ButterflyMax = (1 << 10);

// This is adressed by [side][piece][piece_to][to] (Captures)
const CaptureHistory = [2][6][5][64]i32;
const CaptureMax = (1 << 12);

// This is adressed by [side][piece][to] (Quiets)
const MoveHistory = [2][6][64]i32;
const MoveMax = (1 << 13);

// This is adressed by [side][piece][to][followup_piece][followup_to] (Quiets)
const FollowupHistory = [2][6][64][6][64]i32;
const FollowupMax = (1 << 16);

// This is adressed by [side][pawn_index][piece][to] (Quiets)
const PawnHistorySize = 512;
const PawnHistory = [2][PawnHistorySize][6][64]i32;
const PawnMax = (1 << 14);

// Corrections are statically applied
const CorrSize = 32768;

// This is adressed by [side][pawn_index]
const PawnCorrHistory = [2][CorrSize]i32;
const PawnCorrMax = ev.PawnBase * 2;

// This is adressed by [side][non_pawn_index]
const NonPawnCorrHistory = [2][CorrSize]i32;
const NonPawnCorrMax = ev.PawnBase;

// This is adressed by [side][piece][to] (Based on last move)
const MoveCorrHistory = [2][6][64]i32;
const MoveCorrMax = 3 * ev.PawnBase;

// This is adressed by [side][piece][to][followup_piece][followup_to] (Based on last 2 moves)
const FollowupCorrHistory = [2][6][64][6][64]i32;
const FollowupCorrMax = 5 * ev.PawnBase;

inline fn gravity(max: comptime_int, bonus: i32, prev: i32) i32 {
    const clamp = std.math.clamp(bonus, -max, max);
    return clamp - @divFloor(prev * @as(i32, @intCast(@abs(clamp))), max);
}

pub const Corrections = struct {
    pawn: PawnCorrHistory,
    non_pawn: NonPawnCorrHistory,
    move: MoveCorrHistory,
    followup: FollowupCorrHistory,

    pub inline fn init() Corrections {
        return .{
            .pawn = std.mem.zeroes(PawnCorrHistory),
            .non_pawn = std.mem.zeroes(NonPawnCorrHistory),
            .move = std.mem.zeroes(MoveCorrHistory),
            .followup = std.mem.zeroes(FollowupCorrHistory),
        };
    }

    inline fn pawnIn(self: *const Corrections, b: *const bo.Board) *i32 {
        const side = @intFromEnum(b.side);

        return @constCast(&self.pawn[side][b.getPawnHash() % CorrSize]);
    }

    inline fn nonPawnIn(self: *const Corrections, b: *const bo.Board) *i32 {
        const side = @intFromEnum(b.side);

        return @constCast(&self.non_pawn[side][b.getNonPawnHash() % CorrSize]);
    }

    inline fn moveIn(self: *const Corrections, b: *const bo.Board, move: tp.Move) *i32 {
        const side = @intFromEnum(b.side);
        const piece = @intFromEnum(b.pieceType(move.from));
        const to = @intFromEnum(move.to);

        return @constCast(&self.move[side][piece][to]);
    }

    inline fn followupIn(
        self: *const Corrections,
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
        self: *const Corrections,
        b: *const bo.Board,
        prev: ?tp.Move,
        move: ?tp.Move,
    ) i32 {
        var ret = self.pawnIn(b).* + self.nonPawnIn(b).*;
        if (move) |m| {
            ret += self.moveIn(b, m).*;
            if (prev) |p| ret += self.followupIn(b, p, m).*;
        }

        return ret;
    }

    pub fn update(
        self: *Corrections,
        b: *const bo.Board,
        prev: ?tp.Move,
        move: ?tp.Move,
        depth: i12,
        best: i32,
        expected: i32,
    ) void {
        const bonus = @divFloor((best - expected) * depth, 8);

        const pawn = self.pawnIn(b);
        const non_pawn = self.nonPawnIn(b);
        pawn.* += gravity(PawnCorrMax, bonus, pawn.*);
        non_pawn.* += gravity(NonPawnCorrMax, bonus, non_pawn.*);

        if (move) |m| {
            const mov = self.moveIn(b, m);
            mov.* += gravity(MoveCorrMax, bonus, mov.*);

            if (prev) |p| {
                const followup = self.followupIn(b, p, m);
                followup.* += gravity(FollowupCorrMax, bonus, followup.*);
            }
        }
    }
};

pub const CentiHist = 1 << 4;

pub const Stats = struct {
    butterfly: ButterflyHistory,
    capture: CaptureHistory,
    move: MoveHistory,
    followup: FollowupHistory,
    pawn: PawnHistory,

    pub inline fn init() Stats {
        return .{
            .butterfly = std.mem.zeroes(ButterflyHistory),
            .capture = std.mem.zeroes(CaptureHistory),
            .move = std.mem.zeroes(MoveHistory),
            .followup = std.mem.zeroes(FollowupHistory),
            .pawn = std.mem.zeroes(PawnHistory),
        };
    }

    inline fn butterflyIn(self: *const Stats, b: *const bo.Board, move: tp.Move) *i32 {
        const side = @intFromEnum(b.side);
        const from = @intFromEnum(move.from);
        const to = @intFromEnum(move.to);

        return @constCast(&self.butterfly[side][from][to]);
    }

    inline fn captureIn(self: *const Stats, b: *const bo.Board, move: tp.Move) *i32 {
        const side = @intFromEnum(b.side);
        const piece = @intFromEnum(b.pieceType(move.from));
        const piece_to = @intFromEnum(b.pieceType(move.to));
        const to = @intFromEnum(move.to);

        return @constCast(&self.capture[side][piece][piece_to][to]);
    }

    inline fn moveIn(self: *const Stats, b: *const bo.Board, move: tp.Move) *i32 {
        const side = @intFromEnum(b.side);
        const piece = @intFromEnum(b.pieceType(move.from));
        const to = @intFromEnum(move.to);

        return @constCast(&self.move[side][piece][to]);
    }

    inline fn followupIn(
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

    inline fn pawnIn(self: *const Stats, b: *const bo.Board, move: tp.Move) *i32 {
        const side = @intFromEnum(b.side);
        const piece = @intFromEnum(b.pieceType(move.from));
        const to = @intFromEnum(move.to);

        return @constCast(&self.pawn[side][b.getPawnHash() % PawnHistorySize][piece][to]);
    }

    pub inline fn get(
        self: *const Stats,
        b: *const bo.Board,
        prev: ?tp.Move,
        move: tp.Move,
    ) i32 {
        const quiet = b.isQuiet(move);
        var ret = self.butterflyIn(b, move).*;

        if (!quiet) {
            ret += self.captureIn(b, move).*;
        } else {
            ret += self.moveIn(b, move).*;
            if (prev) |p| ret += self.followupIn(b, p, move).*;
            ret += self.pawnIn(b, move).*;
        }

        return ret;
    }

    pub fn update(
        self: *Stats,
        b: *const bo.Board,
        main: tp.Move,
        list: *const std.ArrayList(tp.Move),
        prev: ?tp.Move,
        depth: i12,
        move_count: u8,
    ) void {
        const big_depth: i32 = @intCast(depth);
        const depth_sq = big_depth * big_depth;

        // Bonuses
        const bonus: i32 = @max(depth_sq * @min(move_count, 16) + 16 * big_depth, 480);

        const quiet = b.isQuiet(main);
        const butterfly = self.butterflyIn(b, main);

        butterfly.* += gravity(ButterflyMax, bonus, butterfly.*);
        if (!quiet) {
            const capture = self.captureIn(b, main);
            capture.* += gravity(ButterflyMax, bonus, capture.*);
        } else {
            const move = self.moveIn(b, main);
            const pawn = self.pawnIn(b, main);
            move.* += gravity(MoveMax, bonus, move.*);
            pawn.* += gravity(PawnMax, bonus, pawn.*);

            if (prev) |p| {
                const followup = self.followupIn(b, p, main);
                followup.* += gravity(FollowupMax, bonus, followup.*);
            }
        }

        const quiet_factor: i3 = @intCast(@intFromBool(quiet));
        // Maluses
        for (0..list.items.len) |i| {
            const m = list.items[i];
            if (m.equals(main)) continue;

            const this_quiet = b.isQuiet(m);
            const this_factor: i3 = @intCast(@intFromBool(this_quiet));
            const malus = @as(i32, @max(
                depth_sq * (5 - @min(move_count, 4)) + big_depth,
                120,
            )) * (1 + quiet_factor - this_factor);

            const this_butterfly = self.butterflyIn(b, m);
            this_butterfly.* += gravity(ButterflyMax, -malus, this_butterfly.*);
            if (!this_quiet) {
                const capture = self.captureIn(b, m);
                capture.* += gravity(ButterflyMax, -malus, capture.*);
            } else {
                const move = self.moveIn(b, m);
                const pawn = self.pawnIn(b, m);
                move.* += gravity(MoveMax, -malus, move.*);
                pawn.* += gravity(PawnMax, -malus, pawn.*);

                if (prev) |p| {
                    const followup = self.followupIn(b, p, m);
                    followup.* += gravity(FollowupMax, -malus, followup.*);
                }
            }
        }
    }
};
