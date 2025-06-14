const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const ev = @import("eval.zig");
const mv = @import("movegen.zig");

pub const SeeValue = [_]i32{
    ev.PawnBase,
    ev.KnightBase,
    ev.BishopBase,
    ev.RookBase,
    ev.QueenBase,
    0,
};
pub inline fn see(
    b: *const bo.Board,
    move: tp.Move,
    gen: *const mv.Maker,
    threshold: i32,
) bool {
    if (b.isQuiet(move) or move.typ == .EnPassant or move.typ.promotion()) return true;

    var val = SeeValue[@intFromEnum(b.pieceType(move.to))] - threshold;
    if (val < 0) return false;

    val = SeeValue[@intFromEnum(b.pieceType(move.from))] - val;
    if (val <= 0) return true;

    var attacks = gen.attackers(move.to);
    var all = b.w_pieces.op_or(b.b_pieces);
    var side = b.side;

    while (true) {
        attacks = attacks.op_and(all);

        const my = attacks.op_and(b.sidePieces(side));
        if (my.v == 0) break;

        var typ: u6 = 0;
        var current = std.mem.zeroes(tp.Square);
        while (typ < 6) : (typ += 1) {
            if (my.op_and(b.typePieces(@enumFromInt(typ))).lsb()) |s| {
                current = s;
                break;
            }
        }

        side.other();
        val = SeeValue[typ] - val;
        if (val >= 0) {
            if (typ == @intFromEnum(tp.PieceType.King) and
                attacks.op_and(b.sidePieces(side)).v != 0) side.other();
            break;
        }

        all = all.without(current.toBoard());

        if (typ == @intFromEnum(tp.PieceType.Pawn) or
            typ == @intFromEnum(tp.PieceType.Bishop) or
            typ == @intFromEnum(tp.PieceType.Queen))
            attacks = attacks.op_or(gen.attackersDiag(move.to, all));
        if (typ == @intFromEnum(tp.PieceType.Rook) or
            typ == @intFromEnum(tp.PieceType.Queen))
            attacks = attacks.op_or(gen.attackersLine(move.to, all));
    }

    return side != b.side;
}
