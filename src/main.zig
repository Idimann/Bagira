const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const pos = @import("positions.zig");

pub fn main() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    const b = try bo.Board.from_fen(pos.start);
    try b.print(stdout);

    try bw.flush();
}
