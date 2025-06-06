const std = @import("std");

pub var ZobristTable = std.mem.zeroes([6][64]u64);
pub var SideHash: u64 = 0;
pub var CastleTable = std.mem.zeroes([4]u64);

pub fn init() !void {
    var prng = std.Random.DefaultPrng.init(blk: {
        var seed: u64 = undefined;
        try std.posix.getrandom(std.mem.asBytes(&seed));
        break :blk seed;
    });
    const rand = prng.random();

    inline for (0..6) |i| {
        inline for (0..64) |j| {
            ZobristTable[i][j] = rand.int(u64);
        }
    }

    SideHash = rand.int(u64);

    inline for (0..4) |i| {
        CastleTable[i] = rand.int(u64);
    }
}
