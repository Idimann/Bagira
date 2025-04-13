const std = @import("std");
const tp = @import("types.zig");

pub fn main() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    var b = tp.BitBoard .new();
    _ = b.set(tp.Square.new(tp.Rank.Rank4, tp.File.FileE));
    try b.print(stdout);
    _ = b.mirror_h();
    try stdout.print("----------------", .{});
    try b.print(stdout);

    try bw.flush();
}
