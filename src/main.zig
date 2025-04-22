const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const pos = @import("positions.zig");
const tab = @import("tablegen.zig");
const gen = @import("movegen.zig");

pub fn main() !void {
    tab.initLines();
    tab.initDiags();

    var b = try bo.Board.fromFen(pos.Testing.castling);
    const b2 = try bo.Board.fromFen(pos.start);

    b.print();
    std.debug.print("{} {}\n", .{ b.hash(), b.hash2() });
    b2.print();
    std.debug.print("{} {}\n", .{ b2.hash(), b2.hash2() });

    var buf: [512 * @sizeOf(tp.Move)]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buf);
    const alloc = fba.allocator();
    var list = std.ArrayList(tp.Move).init(alloc);
    try gen.gen(&b, &list);
    gen.printList(&list);
}
