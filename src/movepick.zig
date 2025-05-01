const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const se = @import("search.zig");

pub fn bestMove(b: *bo.Board, depth: u8) !void {
    const max = std.math.maxInt(i32);

    var buffer: [(1 << 20)]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const alloc = fba.allocator();

    se.setupSearch();
    var dep: u8 = 1; // This seems more efficient than starting at 1 for some reason
    while (dep <= depth) {
        _ = try se.search(
            b,
            -max + @as(i32, dep),
            max,
            dep,
            0,
            @truncate(b.hash_in / 2),
            alloc,
        );

        dep += 1; //if(dep < 6) 2 else 1;
    }
}
