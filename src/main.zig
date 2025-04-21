const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const pos = @import("positions.zig");
const tab = @import("tablegen.zig");

pub fn main() !void {
    tab.initLines();
    tab.initDiags();

    var b = try bo.Board.fromFen(pos.Testing.castling);
    b.print();

    const blockDiag = .{ .v = (b.o_pieces.v | b.t_pieces.v) & tab.getDiagMask(.e4).v };
    const blockLine = .{ .v = (b.o_pieces.v | b.t_pieces.v) & tab.getLineMask(.e4).v };
    const diag = tab.getDiag(.e4, blockDiag);
    const line = tab.getLine(.e4, blockLine);
    diag.print();
    line.print();
}
