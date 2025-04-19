//Most of this is taken from Avalanche (Not the logic, just the numbers)

const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");

pub const KingAttacks = [64]tp.Bitboard{
    .{ .v = 0x302 },
    .{ .v = 0x705 },
    .{ .v = 0xe0a },
    .{ .v = 0x1c14 },
    .{ .v = 0x3828 },
    .{ .v = 0x7050 },
    .{ .v = 0xe0a0 },
    .{ .v = 0xc040 },
    .{ .v = 0x30203 },
    .{ .v = 0x70507 },
    .{ .v = 0xe0a0e },
    .{ .v = 0x1c141c },
    .{ .v = 0x382838 },
    .{ .v = 0x705070 },
    .{ .v = 0xe0a0e0 },
    .{ .v = 0xc040c0 },
    .{ .v = 0x3020300 },
    .{ .v = 0x7050700 },
    .{ .v = 0xe0a0e00 },
    .{ .v = 0x1c141c00 },
    .{ .v = 0x38283800 },
    .{ .v = 0x70507000 },
    .{ .v = 0xe0a0e000 },
    .{ .v = 0xc040c000 },
    .{ .v = 0x302030000 },
    .{ .v = 0x705070000 },
    .{ .v = 0xe0a0e0000 },
    .{ .v = 0x1c141c0000 },
    .{ .v = 0x3828380000 },
    .{ .v = 0x7050700000 },
    .{ .v = 0xe0a0e00000 },
    .{ .v = 0xc040c00000 },
    .{ .v = 0x30203000000 },
    .{ .v = 0x70507000000 },
    .{ .v = 0xe0a0e000000 },
    .{ .v = 0x1c141c000000 },
    .{ .v = 0x382838000000 },
    .{ .v = 0x705070000000 },
    .{ .v = 0xe0a0e0000000 },
    .{ .v = 0xc040c0000000 },
    .{ .v = 0x3020300000000 },
    .{ .v = 0x7050700000000 },
    .{ .v = 0xe0a0e00000000 },
    .{ .v = 0x1c141c00000000 },
    .{ .v = 0x38283800000000 },
    .{ .v = 0x70507000000000 },
    .{ .v = 0xe0a0e000000000 },
    .{ .v = 0xc040c000000000 },
    .{ .v = 0x302030000000000 },
    .{ .v = 0x705070000000000 },
    .{ .v = 0xe0a0e0000000000 },
    .{ .v = 0x1c141c0000000000 },
    .{ .v = 0x3828380000000000 },
    .{ .v = 0x7050700000000000 },
    .{ .v = 0xe0a0e00000000000 },
    .{ .v = 0xc040c00000000000 },
    .{ .v = 0x203000000000000 },
    .{ .v = 0x507000000000000 },
    .{ .v = 0xa0e000000000000 },
    .{ .v = 0x141c000000000000 },
    .{ .v = 0x2838000000000000 },
    .{ .v = 0x5070000000000000 },
    .{ .v = 0xa0e0000000000000 },
    .{ .v = 0x40c0000000000000 },
};

pub const KnightAttacks = [64]tp.Bitboard{
    .{ .v = 0x20400 },
    .{ .v = 0x50800 },
    .{ .v = 0xa1100 },
    .{ .v = 0x142200 },
    .{ .v = 0x284400 },
    .{ .v = 0x508800 },
    .{ .v = 0xa01000 },
    .{ .v = 0x402000 },
    .{ .v = 0x2040004 },
    .{ .v = 0x5080008 },
    .{ .v = 0xa110011 },
    .{ .v = 0x14220022 },
    .{ .v = 0x28440044 },
    .{ .v = 0x50880088 },
    .{ .v = 0xa0100010 },
    .{ .v = 0x40200020 },
    .{ .v = 0x204000402 },
    .{ .v = 0x508000805 },
    .{ .v = 0xa1100110a },
    .{ .v = 0x1422002214 },
    .{ .v = 0x2844004428 },
    .{ .v = 0x5088008850 },
    .{ .v = 0xa0100010a0 },
    .{ .v = 0x4020002040 },
    .{ .v = 0x20400040200 },
    .{ .v = 0x50800080500 },
    .{ .v = 0xa1100110a00 },
    .{ .v = 0x142200221400 },
    .{ .v = 0x284400442800 },
    .{ .v = 0x508800885000 },
    .{ .v = 0xa0100010a000 },
    .{ .v = 0x402000204000 },
    .{ .v = 0x2040004020000 },
    .{ .v = 0x5080008050000 },
    .{ .v = 0xa1100110a0000 },
    .{ .v = 0x14220022140000 },
    .{ .v = 0x28440044280000 },
    .{ .v = 0x50880088500000 },
    .{ .v = 0xa0100010a00000 },
    .{ .v = 0x40200020400000 },
    .{ .v = 0x204000402000000 },
    .{ .v = 0x508000805000000 },
    .{ .v = 0xa1100110a000000 },
    .{ .v = 0x1422002214000000 },
    .{ .v = 0x2844004428000000 },
    .{ .v = 0x5088008850000000 },
    .{ .v = 0xa0100010a0000000 },
    .{ .v = 0x4020002040000000 },
    .{ .v = 0x400040200000000 },
    .{ .v = 0x800080500000000 },
    .{ .v = 0x1100110a00000000 },
    .{ .v = 0x2200221400000000 },
    .{ .v = 0x4400442800000000 },
    .{ .v = 0x8800885000000000 },
    .{ .v = 0x100010a000000000 },
    .{ .v = 0x2000204000000000 },
    .{ .v = 0x4020000000000 },
    .{ .v = 0x8050000000000 },
    .{ .v = 0x110a0000000000 },
    .{ .v = 0x22140000000000 },
    .{ .v = 0x44280000000000 },
    .{ .v = 0x0088500000000000 },
    .{ .v = 0x0010a00000000000 },
    .{ .v = 0x20400000000000 },
};

pub const PawnAttacks = [64]tp.Bitboard{
    .{ .v = 0x200 },
    .{ .v = 0x500 },
    .{ .v = 0xa00 },
    .{ .v = 0x1400 },
    .{ .v = 0x2800 },
    .{ .v = 0x5000 },
    .{ .v = 0xa000 },
    .{ .v = 0x4000 },
    .{ .v = 0x20000 },
    .{ .v = 0x50000 },
    .{ .v = 0xa0000 },
    .{ .v = 0x140000 },
    .{ .v = 0x280000 },
    .{ .v = 0x500000 },
    .{ .v = 0xa00000 },
    .{ .v = 0x400000 },
    .{ .v = 0x2000000 },
    .{ .v = 0x5000000 },
    .{ .v = 0xa000000 },
    .{ .v = 0x14000000 },
    .{ .v = 0x28000000 },
    .{ .v = 0x50000000 },
    .{ .v = 0xa0000000 },
    .{ .v = 0x40000000 },
    .{ .v = 0x200000000 },
    .{ .v = 0x500000000 },
    .{ .v = 0xa00000000 },
    .{ .v = 0x1400000000 },
    .{ .v = 0x2800000000 },
    .{ .v = 0x5000000000 },
    .{ .v = 0xa000000000 },
    .{ .v = 0x4000000000 },
    .{ .v = 0x20000000000 },
    .{ .v = 0x50000000000 },
    .{ .v = 0xa0000000000 },
    .{ .v = 0x140000000000 },
    .{ .v = 0x280000000000 },
    .{ .v = 0x500000000000 },
    .{ .v = 0xa00000000000 },
    .{ .v = 0x400000000000 },
    .{ .v = 0x2000000000000 },
    .{ .v = 0x5000000000000 },
    .{ .v = 0xa000000000000 },
    .{ .v = 0x14000000000000 },
    .{ .v = 0x28000000000000 },
    .{ .v = 0x50000000000000 },
    .{ .v = 0xa0000000000000 },
    .{ .v = 0x40000000000000 },
    .{ .v = 0x200000000000000 },
    .{ .v = 0x500000000000000 },
    .{ .v = 0xa00000000000000 },
    .{ .v = 0x1400000000000000 },
    .{ .v = 0x2800000000000000 },
    .{ .v = 0x5000000000000000 },
    .{ .v = 0xa000000000000000 },
    .{ .v = 0x4000000000000000 },
    .{ .v = 0x0 },
    .{ .v = 0x0 },
    .{ .v = 0x0 },
    .{ .v = 0x0 },
    .{ .v = 0x0 },
    .{ .v = 0x0 },
    .{ .v = 0x0 },
    .{ .v = 0x0 },
};

const RookMagics = [64]tp.BitBoard{
    0x0080001020400080, 0x0040001000200040, 0x0080081000200080, 0x0080040800100080,
    0x0080020400080080, 0x0080010200040080, 0x0080008001000200, 0x0080002040800100,
    0x0000800020400080, 0x0000400020005000, 0x0000801000200080, 0x0000800800100080,
    0x0000800400080080, 0x0000800200040080, 0x0000800100020080, 0x0000800040800100,
    0x0000208000400080, 0x0000404000201000, 0x0000808010002000, 0x0000808008001000,
    0x0000808004000800, 0x0000808002000400, 0x0000010100020004, 0x0000020000408104,
    0x0000208080004000, 0x0000200040005000, 0x0000100080200080, 0x0000080080100080,
    0x0000040080080080, 0x0000020080040080, 0x0000010080800200, 0x0000800080004100,
    0x0000204000800080, 0x0000200040401000, 0x0000100080802000, 0x0000080080801000,
    0x0000040080800800, 0x0000020080800400, 0x0000020001010004, 0x0000800040800100,
    0x0000204000808000, 0x0000200040008080, 0x0000100020008080, 0x0000080010008080,
    0x0000040008008080, 0x0000020004008080, 0x0000010002008080, 0x0000004081020004,
    0x0000204000800080, 0x0000200040008080, 0x0000100020008080, 0x0000080010008080,
    0x0000040008008080, 0x0000020004008080, 0x0000800100020080, 0x0000800041000080,
    0x00FFFCDDFCED714A, 0x007FFCDDFCED714A, 0x003FFFCDFFD88096, 0x0000040810002101,
    0x0001000204080011, 0x0001000204000801, 0x0001000082000401, 0x0001FFFAABFAD1A2,
};
var RookShifts = std.mem.zeroes([64]usize);
pub var RookAttacks = std.mem.zeroes([64][4096]tp.BitBoard);

// https://analog-hors.github.io/site/magic-bitboards/
pub fn initRooks() void {
    var sq = @as(usize, @intFromEnum(tp.Square.a1));

    const noRank = .{ .v = tp.fileMask(.FileA).v | tp.fileMask(.FileH).v };
    const noFile = .{ .v = tp.rankMask(.Rank1).v | tp.rankMask(.Rank8).v };

    while (sq <= @intFromEnum(tp.Square.h8)) : (sq += 1) {
        const square: tp.Square = @enumFromInt(sq);
        const mask = (tp.BitBoard { .v = tp.rankMask(square.rank()).without(noRank).v |
            tp.fileMask(square.file()).without(noFile).v }).without(square.toBoard());

        RookShifts[sq] = 64 - mask.popcount();

        // var ran: u64 = 0;
        // while (ran < 1<<6) : (ran += 1) {
        //     var fil: u64 = 0;
        //     while (fil < 1<<6) : (fil += 1) {
        //         const pie =
        //         (ran << (@intFromEnum(square.rank()) * 8 + 1))
        //         | (fil << (@intFromEnum(square.file()) + 1));
        //     }
        // }
    }
}
