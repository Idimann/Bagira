const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");

const MaxEval: f16 = 100;
fn eval_part(b: *const bo.Board) f16 {
    var ret: f16 = 0;
    var iter = b.o_pieces;
    while (iter.popLsb()) |sq| {
        if (b.diags.check(sq)) {
            ret += if (b.lines.check(sq)) 9 else 3.5;
        }
        else if (b.lines.check(sq)) ret += 5
        else if (b.o_king != sq) ret += 3;
    }

    return ret;
}

pub fn eval(b: *bo.Board) f16 {
    const our = eval_part(b);
    const their = eval_part(b.mirror());
    _ = b.mirror();
    return (our - their) / MaxEval;
}
