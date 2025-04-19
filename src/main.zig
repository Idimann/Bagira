const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const pos = @import("positions.zig");
const tab = @import("tablegen.zig");

pub fn main() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    tab.initRooks();

    var b = try bo.Board.fromFen(pos.start);

    const mo1 = .{ .from = .e2, .to = .e4, .typ = .Normal };
    const un1 = b.apply(mo1);
    const mo2 = .{ .from = .e2, .to = .e4, .typ = .Normal };
    const un2 = b.apply(mo2);
    const mo3 = .{ .from = .g1, .to = .f3, .typ = .Normal };
    const un3 = b.apply(mo3);
    const mo4 = .{ .from = .b1, .to = .c3, .typ = .Normal };
    const un4 = b.apply(mo4);

    _ = try b.remove(mo4, un4);
    _ = try b.remove(mo3, un3);
    _ = try b.remove(mo2, un2);
    _ = try b.remove(mo1, un1);

    try b.print(stdout);
    try b.enPassant().print(stdout);

    try bw.flush();
}
