const std = @import("std");
const ta = @import("tablegen.zig");
const tp = @import("types.zig");
const bo = @import("board.zig");

const PawnBase = 100;
const PawnRank = [_]i32{ 0, 0, -4, 8, 10, 11, 17, 0 };
const PawnFile = [_]i32{ 2, 0, 1, 4, 4, 1, 0, 2 };
const PawnHold = 3;
fn eval_pawn(b: *const bo.Board, sq: tp.Square) i32 {
    var ret: i32 = PawnBase;

    const rank = sq.rank();
    const file = sq.file();
    ret += PawnRank[@intFromEnum(rank)] + PawnFile[@intFromEnum(file)];

    if (sq.getApplySafe(.NorthEast)) |t| {
        if (b.o_pieces.check(t)) ret += PawnHold;
    }
    if (sq.getApplySafe(.NorthWest)) |t| {
        if (b.o_pieces.check(t)) ret += PawnHold;
    }

    return ret;
}

const KnightBase = 305;
const KnightRank = [_]i32{ 0, 2, 3, 3, 6, 6, 5, 3 };
const KnightFile = [_]i32{ 0, 0, 3, 4, 4, 3, 0, 0 };
const KnightAttack = 5;
const KnightFree = [_]i32{ -20, -13, -5, 0, 3, 8, 9, 11, 15 };
fn eval_knight(b: *const bo.Board, sq: tp.Square) i32 {
    var ret: i32 = KnightBase;

    const rank = sq.rank();
    const file = sq.file();
    ret += KnightRank[@intFromEnum(rank)] + KnightFile[@intFromEnum(file)];

    ret += @intCast(KnightAttack *
        ta.KnightAttacks[@intFromEnum(sq)].op_and(b.t_pieces).popcount());
    ret += KnightFree[ta.KnightAttacks[@intFromEnum(sq)].without(b.o_pieces).popcount()];

    return ret;
}

const BishopBase = 335;
const BishopRowCount = [_]i32{ -6, -3, 0, 1, 2, 4, 10, 15, 25 };
const BishopRowFree = [_]i32{ -5, -2, 0, 3, 5, 11, 20, 30 };
fn eval_bishop(b: *const bo.Board, sq: tp.Square) i32 {
    var ret: i32 = BishopBase;

    const diag = sq.diagonal();
    const anti = sq.antiDiagonal();

    const goto = ta.getDiag(sq, b.o_pieces.op_or(b.t_pieces).op_and(ta.getDiagMask(sq)))
        .without(b.o_pieces);
    ret += BishopRowFree[goto.op_and(tp.DiagonalMask[diag]).popcount()];
    ret += BishopRowFree[goto.op_and(tp.AntiDiagonalMask[anti]).popcount()];

    ret += BishopRowCount[tp.DiagonalMask[diag].popcount()];
    ret += BishopRowCount[tp.AntiDiagonalMask[anti].popcount()];

    return ret;
}

const RookBase = 495;
const RookRank = [_]i32{ 0, 2, 2, 3, 5, 7, 16, 17 };
const RookFile = [_]i32{ -2, 0, 3, 6, 6, 3, 0, -2 };
const RookRankFree = [_]i32{ -1, 0, 0, 1, 1, 2, 3, 4 };
const RookFileFree = [_]i32{ -4, -3, 0, 3, 8, 13, 17, 22 };
fn eval_rook(b: *const bo.Board, sq: tp.Square) i32 {
    var ret: i32 = RookBase;

    const rank = sq.rank();
    const file = sq.file();
    ret += RookRank[@intFromEnum(rank)] + RookFile[@intFromEnum(file)];

    const goto = ta.getLine(sq, b.o_pieces.op_or(b.t_pieces).op_and(ta.getLineMask(sq)))
        .without(b.o_pieces);
    ret += RookRankFree[goto.op_and(tp.RankMask[@intFromEnum(rank)]).popcount()];
    ret += RookFileFree[goto.op_and(tp.FileMask[@intFromEnum(file)]).popcount()];

    return ret;
}

const KingRank = [_]i32{ 10, 0, -20, -50, -130, -250, -260, -300 };
const KingFile = [_]i32{ 15, 10, 0, -7, -5, 0, 10, 15 };
fn eval_king(b: *const bo.Board) i32 {
    var ret: i32 = RookBase;

    const rank = b.o_king.rank();
    const file = b.o_king.file();
    ret += KingRank[@intFromEnum(rank)] + KingFile[@intFromEnum(file)];

    return ret;
}

fn eval_part(b: *const bo.Board) i32 {
    var ret = eval_king(b);
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

pub fn eval(b: *bo.Board) i32 {
    const our = eval_part(b);
    const their = eval_part(b.mirror());
    _ = b.mirror();
    return our - their;
}
