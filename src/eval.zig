const std = @import("std");
const ta = @import("tablegen.zig");
const tp = @import("types.zig");
const bo = @import("board.zig");
const mv = @import("movegen.zig");

pub const PawnBase = 100;
const PawnRank = [_]i32{ 0, 0, -4, 8, 10, 11, 17, 0 };
const PawnFile = [_]i32{ 2, 0, 1, 4, 4, 1, 0, 2 };
const PawnHold = 3;
fn eval_pawn(b: *const bo.Board, dat: *const mv.Data, sq: tp.Square, side: bo.Side) i32 {
    _ = b;
    var ret: i32 = PawnBase;

    const rank: tp.Rank = if (side == .White)
        sq.rank()
    else
        @enumFromInt(7 - @intFromEnum(sq.rank()));
    const file = sq.file();
    ret += PawnRank[@intFromEnum(rank)] + PawnFile[@intFromEnum(file)];
    const iter = (if (side == .White)
        ta.PawnAttacksWhite[@intFromEnum(sq)]
    else
        ta.PawnAttacksBlack[@intFromEnum(sq)]).op_and(dat.their);

    ret += PawnHold * @as(i32, @intCast(iter.popcount()));
    return ret;
}

pub const KnightBase = 305;
const KnightRank = [_]i32{ 0, 2, 3, 3, 6, 6, 5, 3 };
const KnightFile = [_]i32{ 0, 0, 3, 4, 4, 3, 0, 0 };
const KnightAttack = 5;
const KnightFree = [_]i32{ -20, -13, -5, 0, 3, 8, 9, 11, 15 };
fn eval_knight(b: *const bo.Board, dat: *const mv.Data, sq: tp.Square, side: bo.Side) i32 {
    _ = b;
    var ret: i32 = KnightBase;

    const rank: tp.Rank = if (side == .White)
        sq.rank()
    else
        @enumFromInt(7 - @intFromEnum(sq.rank()));
    const file = sq.file();
    ret += KnightRank[@intFromEnum(rank)] + KnightFile[@intFromEnum(file)];

    ret += @intCast(KnightAttack *
        ta.KnightAttacks[@intFromEnum(sq)].op_and(dat.their).popcount());
    ret += KnightFree[ta.KnightAttacks[@intFromEnum(sq)].without(dat.our).popcount()];

    return ret;
}

pub const BishopBase = 335;
const BishopRowCount = [_]i32{ -6, -3, 0, 1, 2, 4, 10, 15, 25 };
const BishopRowFree = [_]i32{ -5, -2, 0, 3, 5, 11, 20, 30 };
fn eval_bishop(b: *const bo.Board, dat: *const mv.Data, sq: tp.Square) i32 {
    _ = b;
    var ret: i32 = BishopBase;

    const diag = sq.diagonal();
    const anti = sq.antiDiagonal();

    const goto = ta.getDiag(sq, dat.combi).without(dat.our);
    ret += BishopRowFree[goto.op_and(tp.DiagonalMask[diag]).popcount()];
    ret += BishopRowFree[goto.op_and(tp.AntiDiagonalMask[anti]).popcount()];

    ret += BishopRowCount[tp.DiagonalMask[diag].popcount()];
    ret += BishopRowCount[tp.AntiDiagonalMask[anti].popcount()];

    return ret;
}

pub const RookBase = 495;
const RookRank = [_]i32{ 0, 2, 2, 3, 5, 7, 16, 17 };
const RookFile = [_]i32{ -2, 0, 3, 6, 6, 3, 0, -2 };
const RookRankFree = [_]i32{ -1, 0, 0, 1, 1, 2, 3, 4 };
const RookFileFree = [_]i32{ -4, -3, 0, 3, 8, 13, 17, 22 };
fn eval_rook(b: *const bo.Board, dat: *const mv.Data, sq: tp.Square, side: bo.Side) i32 {
    _ = b;
    var ret: i32 = RookBase;

    const rank: tp.Rank = if (side == .White)
        sq.rank()
    else
        @enumFromInt(7 - @intFromEnum(sq.rank()));
    const file = sq.file();
    ret += RookRank[@intFromEnum(rank)] + RookFile[@intFromEnum(file)];

    const goto = ta.getLine(sq, dat.combi).without(dat.our);
    ret += RookRankFree[goto.op_and(tp.RankMask[@intFromEnum(rank)]).popcount()];
    ret += RookFileFree[goto.op_and(tp.FileMask[@intFromEnum(file)]).popcount()];

    return ret;
}

pub const QueenBase = 965;
const QueenRank = [_]i32{ 0, 5, -1, 3, 5, 7, 8, 9 };
const QueenFile = [_]i32{ 3, 5, 6, 1, 1, 6, 5, 3 };
fn eval_queen(b: *const bo.Board, dat: *const mv.Data, sq: tp.Square, side: bo.Side) i32 {
    _ = b;
    _ = dat;
    var ret: i32 = QueenBase;

    const rank: tp.Rank = if (side == .White)
        sq.rank()
    else
        @enumFromInt(7 - @intFromEnum(sq.rank()));
    const file = sq.file();
    ret += QueenRank[@intFromEnum(rank)] + QueenFile[@intFromEnum(file)];

    return ret;
}

const KingRank = [_]i32{ 10, 0, -20, -50, -130, -250, -260, -300 };
const KingFile = [_]i32{ 25, 20, 10, -7, -5, 0, 20, 25 };
const OpenMalus: i32 = -60;
const CastleBonus: i32 = 17;
fn eval_king(b: *const bo.Board, dat: *const mv.Data, side: bo.Side) i32 {
    var ret: i32 = 0;

    const rank: tp.Rank = if (side == .White)
        dat.our_king.rank()
    else
        @enumFromInt(7 - @intFromEnum(dat.our_king.rank()));
    const file = dat.our_king.file();
    ret += KingRank[@intFromEnum(rank)] + KingFile[@intFromEnum(file)];

    // Being in front of the pawns is evaluated as good by this, that should be taken care of
    // by the rank mali though (in most cases)
    const pop: i32 = @intCast(tp.FileMask[@intFromEnum(file)].op_and(b.pawns).popcount());
    if (pop <= 1) ret -= OpenMalus * (2 - pop);

    if (side == .White) {
        if (b.castle.wk) ret += CastleBonus;
        if (b.castle.wq) ret += CastleBonus;
    } else {
        if (b.castle.bk) ret += CastleBonus;
        if (b.castle.bq) ret += CastleBonus;
    }

    return ret;
}

fn eval_part(b: *const bo.Board, side: bo.Side) i32 {
    const dat: mv.Data = .{
        .combi = b.w_pieces.op_or(b.b_pieces),
        .our = if (side == .White) b.w_pieces else b.b_pieces,
        .their = if (side == .White) b.b_pieces else b.w_pieces,
        .our_king = if (side == .White) b.w_king else b.b_king,
    };
    var ret: i32 = eval_king(b, &dat, side);
    var iter = dat.our;
    while (iter.popLsb()) |sq| {
        if (b.diags.check(sq)) {
            if (b.lines.check(sq))
                ret += eval_queen(b, &dat, sq, side)
            else
                ret += eval_bishop(b, &dat, sq);
        } else if (b.lines.check(sq))
            ret += eval_rook(b, &dat, sq, side)
        else if (b.pawns.check(sq))
            ret += eval_pawn(b, &dat, sq, side)
        else if (dat.our_king != sq)
            ret += eval_knight(b, &dat, sq, side);
    }

    return ret;
}

pub fn eval(b: *bo.Board) i32 {
    const white = eval_part(b, .White);
    const black = eval_part(b, .Black);
    return if (b.side == .White) white - black else black - white;
}
