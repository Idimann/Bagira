const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const ta = @import("tablegen.zig");
const mv = @import("movegen.zig");
const nn = @import("nn.zig");

pub const PawnBase = nn.SCALE;
pub const CentiPawn: i32 = @divExact(PawnBase, 10);
pub const KnightBase = CentiPawn * 31;
pub const BishopBase = CentiPawn * 33;
pub const RookBase = CentiPawn * 50;
pub const QueenBase = CentiPawn * 95;
pub const PieceValue = [_]i32{
    PawnBase,
    KnightBase,
    BishopBase,
    RookBase,
    QueenBase,
};

pub inline fn adjust(input: i32, b: *const bo.Board) i32 {
    var ret = input;

    const move_rule_adjust: f32 = (150 - @as(f32, @floatFromInt(b.move_rule))) / 150;
    ret *= @intFromFloat(1 + move_rule_adjust);

    return ret;
}
