const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const ta = @import("tablegen.zig");
const mv = @import("movegen.zig");

pub const PawnBase = 100;
pub const KnightBase = 305;
pub const BishopBase = 335;
pub const RookBase = 495;
pub const QueenBase = 965;
pub const CentiPawn: i32 = @divExact(PawnBase, 10);

pub inline fn adjust(input: i32, b: *const bo.Board) i32 {
    var ret = input;

    const move_rule_adjust: f32 = (150 - @as(f32, @floatFromInt(b.move_rule))) / 150;
    ret *= @intFromFloat(1 + move_rule_adjust);

    return ret;
}
