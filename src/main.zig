const std = @import("std");

pub fn main() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();
    try stdout.print("Hello joe\n", .{});

    const x: u64 = 0b10000;
    const foo: i32 = @intCast(@ctz(x));
    try stdout.print("{}\n", .{foo});

    try bw.flush();
}
