const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const pos = @import("positions.zig");

pub fn main() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    var b = try bo.Board.from_fen(pos.Testing.castling);
    const undo = b.apply(.{ .from = .e1, .to = .c1, .typ = .CastleQueenside });
    try stdout.print("{?}\n", .{undo});
    try b.print(stdout);
    try b.en_passant().print(stdout);

    try bw.flush();
}
