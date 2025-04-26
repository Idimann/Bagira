const std = @import("std");
const ta = @import("tablegen.zig");
const tp = @import("types.zig");
const bo = @import("board.zig");

const PawnBase = 1.0;
const PawnRank = [_]f16{ 0, 1, 0.93, 1.15, 1.05, 1.3, 1.65, 0 };
const PawnFile = [_]f16{ 1.05, 0.96, 1, 1.1, 1.1, 1, 0.96, 1.05 };
const PawnHold = 1.15;
fn eval_pawn(b: *const bo.Board, sq: tp.Square) f16 {
    var ret: f16 = PawnBase;

    const rank = sq.rank();
    const file = sq.file();
    ret *= PawnRank[@intFromEnum(rank)] * PawnFile[@intFromEnum(file)];

    if (sq.getApplySafe(.NorthEast)) |t| {
        if (b.o_pieces.check(t)) ret *= PawnHold;
    }
    if (sq.getApplySafe(.NorthWest)) |t| {
        if (b.o_pieces.check(t)) ret *= PawnHold;
    }

    return ret;
}

const KnightBase = 3.05;
const KnightRank = [_]f16{ 1, 1.1, 1.15, 1.05, 1.18, 1.35, 1.2, 1.1 };
const KnightFile = [_]f16{ 0.9, 0.96, 1, 1.1, 1.1, 1, 0.96, 0.9 };
const KnightAttack = 1.1;
const KnightFree = [_]f16{ 0.55, 0.75, 0.9, 1, 1.05, 1.1, 1.25, 1.3, 1.4 };
fn eval_knight(b: *const bo.Board, sq: tp.Square) f16 {
    var ret: f16 = KnightBase;

    const rank = sq.rank();
    const file = sq.file();
    ret *= KnightRank[@intFromEnum(rank)] * KnightFile[@intFromEnum(file)];

    ret *= @floatCast(std.math.pow(
        f32,
        KnightAttack,
        @floatFromInt(ta.KnightAttacks[@intFromEnum(sq)].op_and(b.t_pieces).popcount()),
    ));
    ret *= KnightFree[ta.KnightAttacks[@intFromEnum(sq)].without(b.o_pieces).popcount()];

    return ret;
}

const BishopBase = 3.45;
const BishopRowCount = [_]f16{ 0.93, 0.96, 1, 1.02, 1.03, 1.05, 1.08, 1.15, 1.21 };
const BishopRowFree = [_]f16{ 0.89, 0.95, 1, 1.05, 1.12, 1.16, 1.22, 1.32 };
fn eval_bishop(b: *const bo.Board, sq: tp.Square) f16 {
    var ret: f16 = BishopBase;

    const diag = sq.diagonal();
    const anti = sq.antiDiagonal();

    const goto = ta.getDiag(sq, b.o_pieces.op_or(b.t_pieces).op_and(ta.getDiagMask(sq)))
        .without(b.o_pieces);
    ret *= BishopRowFree[goto.op_and(tp.DiagonalMask[diag]).popcount()];
    ret *= BishopRowFree[goto.op_and(tp.AntiDiagonalMask[anti]).popcount()];

    ret *= BishopRowCount[tp.DiagonalMask[diag].popcount()];
    ret *= BishopRowCount[tp.AntiDiagonalMask[anti].popcount()];

    return ret;
}

const RookBase = 4.95;
const RookRank = [_]f16{ 1, 1, 1.01, 0.95, 0.9, 1.03, 1.3, 1.1 };
const RookFile = [_]f16{ 0.95, 1, 1.05, 1.12, 1.12, 1.05, 1, 0.95 };
const RookRankFree = [_]f16{ 0.96, 0.97, 1, 1.01, 1.02, 1.04, 1.07, 1.1 };
const RookFileFree = [_]f16{ 0.9, 0.98, 1, 1.08, 1.18, 1.2, 1.3, 1.36 };
fn eval_rook(b: *const bo.Board, sq: tp.Square) f16 {
    var ret: f16 = RookBase;

    const rank = sq.rank();
    const file = sq.file();
    ret *= RookRank[@intFromEnum(rank)] * RookFile[@intFromEnum(file)];

    const goto = ta.getLine(sq, b.o_pieces.op_or(b.t_pieces).op_and(ta.getLineMask(sq)))
        .without(b.o_pieces);
    ret *= RookRankFree[goto.op_and(tp.RankMask[@intFromEnum(rank)]).popcount()];
    ret *= RookFileFree[goto.op_and(tp.FileMask[@intFromEnum(file)]).popcount()];

    return ret;
}

fn eval_part(b: *const bo.Board) f16 {
    var ret: f16 = 0;
    var iter = b.o_pieces;
    while (iter.popLsb()) |sq| {
        if (b.diags.check(sq)) {
            ret += eval_bishop(b, sq);
            if (b.lines.check(sq))
                ret += eval_rook(b, sq);
        } else if (b.lines.check(sq))
            ret += eval_rook(b, sq)
        else if (b.pawns.check(sq))
            ret += eval_pawn(b, sq)
        else if (b.o_king != sq)
            ret += eval_knight(b, sq);
    }

    return ret;
}

pub const EvalMax = 100.0;
pub fn eval(b: *bo.Board) f16 {
    const our = eval_part(b);
    const their = eval_part(b.mirror());
    _ = b.mirror();
    return std.math.clamp((our - their) / EvalMax, -1, 1);
}
