//Most of this is taken from Avalanche (Not the logic, just the numbers)
// https://analog-hors.github.io/site/magic-bitboards/

const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");

pub const KingAttacks = [64]tp.BitBoard{
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

pub const KnightAttacks = [64]tp.BitBoard{
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

pub const PawnAttacks = [64]tp.BitBoard{
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

const LineMagics = [64]tp.BitBoard{
    .{ .v = 0x0080001020400080 },
    .{ .v = 0x0040001000200040 },
    .{ .v = 0x0080081000200080 },
    .{ .v = 0x0080040800100080 },
    .{ .v = 0x0080020400080080 },
    .{ .v = 0x0080010200040080 },
    .{ .v = 0x0080008001000200 },
    .{ .v = 0x0080002040800100 },
    .{ .v = 0x0000800020400080 },
    .{ .v = 0x0000400020005000 },
    .{ .v = 0x0000801000200080 },
    .{ .v = 0x0000800800100080 },
    .{ .v = 0x0000800400080080 },
    .{ .v = 0x0000800200040080 },
    .{ .v = 0x0000800100020080 },
    .{ .v = 0x0000800040800100 },
    .{ .v = 0x0000208000400080 },
    .{ .v = 0x0000404000201000 },
    .{ .v = 0x0000808010002000 },
    .{ .v = 0x0000808008001000 },
    .{ .v = 0x0000808004000800 },
    .{ .v = 0x0000808002000400 },
    .{ .v = 0x0000010100020004 },
    .{ .v = 0x0000020000408104 },
    .{ .v = 0x0000208080004000 },
    .{ .v = 0x0000200040005000 },
    .{ .v = 0x0000100080200080 },
    .{ .v = 0x0000080080100080 },
    .{ .v = 0x0000040080080080 },
    .{ .v = 0x0000020080040080 },
    .{ .v = 0x0000010080800200 },
    .{ .v = 0x0000800080004100 },
    .{ .v = 0x0000204000800080 },
    .{ .v = 0x0000200040401000 },
    .{ .v = 0x0000100080802000 },
    .{ .v = 0x0000080080801000 },
    .{ .v = 0x0000040080800800 },
    .{ .v = 0x0000020080800400 },
    .{ .v = 0x0000020001010004 },
    .{ .v = 0x0000800040800100 },
    .{ .v = 0x0000204000808000 },
    .{ .v = 0x0000200040008080 },
    .{ .v = 0x0000100020008080 },
    .{ .v = 0x0000080010008080 },
    .{ .v = 0x0000040008008080 },
    .{ .v = 0x0000020004008080 },
    .{ .v = 0x0000010002008080 },
    .{ .v = 0x0000004081020004 },
    .{ .v = 0x0000204000800080 },
    .{ .v = 0x0000200040008080 },
    .{ .v = 0x0000100020008080 },
    .{ .v = 0x0000080010008080 },
    .{ .v = 0x0000040008008080 },
    .{ .v = 0x0000020004008080 },
    .{ .v = 0x0000800100020080 },
    .{ .v = 0x0000800041000080 },
    .{ .v = 0x00FFFCDDFCED714A },
    .{ .v = 0x007FFCDDFCED714A },
    .{ .v = 0x003FFFCDFFD88096 },
    .{ .v = 0x0000040810002101 },
    .{ .v = 0x0001000204080011 },
    .{ .v = 0x0001000204000801 },
    .{ .v = 0x0001000082000401 },
    .{ .v = 0x0001FFFAABFAD1A2 },
};
var LineShifts = std.mem.zeroes([64]usize);
var LineMasks = std.mem.zeroes([64]tp.BitBoard);
var LineAttacks = std.mem.zeroes([64][4096]tp.BitBoard);

fn toFile(i: u64, f: tp.File) u64 {
    var ret: u64 = 0;

    var iter = tp.FileMask[@intFromEnum(f)];
    var count: u4 = 0;
    while (iter.popLsb()) |sq| : (count += 1) {
        if (i & (@as(u8, 1) << @intCast(count)) != 0) {
            ret |= sq.toBoard().v;
        }
    }

    return ret;
}

fn getLineAttacks(sq: tp.Square, block: tp.BitBoard) tp.BitBoard {
    var ret: tp.BitBoard = .{ .v = 0 };
    const rank = @intFromEnum(sq.rank());
    const file = @intFromEnum(sq.file());

    for ((rank + 1)..8) |r| {
        const s = tp.Square.new(@enumFromInt(r), sq.file());
        _ = ret.set(s);
        if (block.check(s))
            break;
    }
    for (0..rank) |r| {
        const s = tp.Square.new(@enumFromInt(rank - 1 - r), sq.file());
        _ = ret.set(s);
        if (block.check(s))
            break;
    }
    for ((file + 1)..8) |f| {
        const s = tp.Square.new(sq.rank(), @enumFromInt(f));
        _ = ret.set(s);
        if (block.check(s))
            break;
    }
    for (0..file) |f| {
        const s = tp.Square.new(sq.rank(), @enumFromInt(file - 1 - f));
        _ = ret.set(s);
        if (block.check(s))
            break;
    }

    return ret;
}

pub fn initLines() void {
    const noRank = tp.FileMask[@intFromEnum(tp.File.FileA)]
        .o(tp.FileMask[@intFromEnum(tp.File.FileH)]);
    const noFile = tp.RankMask[@intFromEnum(tp.Rank.Rank1)]
        .o(tp.RankMask[@intFromEnum(tp.Rank.Rank8)]);

    for (0..64) |sq| {
        const square: tp.Square = @enumFromInt(sq);
        const mask = (tp.RankMask[@intFromEnum(square.rank())].without(noRank))
            .o(tp.FileMask[@intFromEnum(square.file())].without(noFile))
            .without(square.toBoard());

        LineShifts[sq] = 64 - mask.popcount();
        LineMasks[sq] = mask;

        for (0..(1 << 6)) |ran| {
            for (0..(1 << 6)) |fil| {
                const rank = ran << 1;
                const file = fil << 1;
                const pie: tp.BitBoard = .{ .v = (rank << (@intFromEnum(square.rank()) * 8)) |
                    toFile(file, square.file()) };

                if (pie.check(square)) continue;

                const index = (pie.v *% LineMagics[sq].v) >> @intCast(LineShifts[sq]);
                LineAttacks[sq][index] = getLineAttacks(square, pie);
            }
        }
    }
}

pub inline fn getLineMask(s: tp.Square) tp.BitBoard {
    return LineMasks[@intFromEnum(s)];
}

pub inline fn getLine(s: tp.Square, bl: tp.BitBoard) tp.BitBoard {
    const shift = LineShifts[@intFromEnum(s)];
    const magic = LineMagics[@intFromEnum(s)];
    const index = (bl.v *% magic.v) >> @intCast(shift);
    return LineAttacks[@intFromEnum(s)][index];
}

const DiagMagics = [64]tp.BitBoard{
    .{ .v = 0x0002020202020200 },
    .{ .v = 0x0002020202020000 },
    .{ .v = 0x0004010202000000 },
    .{ .v = 0x0004040080000000 },
    .{ .v = 0x0001104000000000 },
    .{ .v = 0x0000821040000000 },
    .{ .v = 0x0000410410400000 },
    .{ .v = 0x0000104104104000 },
    .{ .v = 0x0000040404040400 },
    .{ .v = 0x0000020202020200 },
    .{ .v = 0x0000040102020000 },
    .{ .v = 0x0000040400800000 },
    .{ .v = 0x0000011040000000 },
    .{ .v = 0x0000008210400000 },
    .{ .v = 0x0000004104104000 },
    .{ .v = 0x0000002082082000 },
    .{ .v = 0x0004000808080800 },
    .{ .v = 0x0002000404040400 },
    .{ .v = 0x0001000202020200 },
    .{ .v = 0x0000800802004000 },
    .{ .v = 0x0000800400A00000 },
    .{ .v = 0x0000200100884000 },
    .{ .v = 0x0000400082082000 },
    .{ .v = 0x0000200041041000 },
    .{ .v = 0x0002080010101000 },
    .{ .v = 0x0001040008080800 },
    .{ .v = 0x0000208004010400 },
    .{ .v = 0x0000404004010200 },
    .{ .v = 0x0000840000802000 },
    .{ .v = 0x0000404002011000 },
    .{ .v = 0x0000808001041000 },
    .{ .v = 0x0000404000820800 },
    .{ .v = 0x0001041000202000 },
    .{ .v = 0x0000820800101000 },
    .{ .v = 0x0000104400080800 },
    .{ .v = 0x0000020080080080 },
    .{ .v = 0x0000404040040100 },
    .{ .v = 0x0000808100020100 },
    .{ .v = 0x0001010100020800 },
    .{ .v = 0x0000808080010400 },
    .{ .v = 0x0000820820004000 },
    .{ .v = 0x0000410410002000 },
    .{ .v = 0x0000082088001000 },
    .{ .v = 0x0000002011000800 },
    .{ .v = 0x0000080100400400 },
    .{ .v = 0x0001010101000200 },
    .{ .v = 0x0002020202000400 },
    .{ .v = 0x0001010101000200 },
    .{ .v = 0x0000410410400000 },
    .{ .v = 0x0000208208200000 },
    .{ .v = 0x0000002084100000 },
    .{ .v = 0x0000000020880000 },
    .{ .v = 0x0000001002020000 },
    .{ .v = 0x0000040408020000 },
    .{ .v = 0x0004040404040000 },
    .{ .v = 0x0002020202020000 },
    .{ .v = 0x0000104104104000 },
    .{ .v = 0x0000002082082000 },
    .{ .v = 0x0000000020841000 },
    .{ .v = 0x0000000000208800 },
    .{ .v = 0x0000000010020200 },
    .{ .v = 0x0000000404080200 },
    .{ .v = 0x0000040404040400 },
    .{ .v = 0x0002020202020200 },
};
var DiagShifts = std.mem.zeroes([64]usize);
var DiagMasks = std.mem.zeroes([64]tp.BitBoard);
var DiagAttacks = std.mem.zeroes([64][4096]tp.BitBoard);

fn toDiagonal(i: u64, d: u6) u64 {
    var ret: u64 = 0;

    var iter = tp.DiagonalMask[d];
    var count: u4 = 0;
    while (iter.popLsb()) |sq| : (count += 1) {
        if (i & (@as(u8, 1) << @intCast(count)) != 0) {
            ret |= sq.toBoard().v;
        }
    }

    return ret;
}

fn toAntiDiagonal(i: u64, d: u6) u64 {
    var ret: u64 = 0;

    var iter = tp.AntiDiagonalMask[d];
    var count: u4 = 0;
    while (iter.popLsb()) |sq| : (count += 1) {
        if (i & (@as(u8, 1) << @intCast(count)) != 0) {
            ret |= sq.toBoard().v;
        }
    }

    return ret;
}

fn getDiagAttacks(sq: tp.Square, block: tp.BitBoard) tp.BitBoard {
    var ret: tp.BitBoard = .{ .v = 0 };
    const rank = @intFromEnum(sq.rank());
    const file = @intFromEnum(sq.file());

    for (@max(rank + 1, file + 1)..8) |c| {
        const val = c - @max(rank, file);
        const s = tp.Square.new(@enumFromInt(rank + val), @enumFromInt(file + val));
        _ = ret.set(s);
        if (block.check(s))
            break;
    }
    for (0..@min(rank, file)) |c| {
        const val = c + 1;
        const s = tp.Square.new(@enumFromInt(rank - val), @enumFromInt(file - val));
        _ = ret.set(s);
        if (block.check(s))
            break;
    }
    for (@max(7 - rank, file + 1)..8) |c| {
        const val = c - @max(7 - rank, file);
        const s = tp.Square.new(@enumFromInt(rank - val), @enumFromInt(file + val));
        _ = ret.set(s);
        if (block.check(s))
            break;
    }
    for (@max(rank + 1, 7 - file)..8) |c| {
        const val = c - @max(rank, 7 - file);
        const s = tp.Square.new(@enumFromInt(rank + val), @enumFromInt(file - val));
        _ = ret.set(s);
        if (block.check(s))
            break;
    }

    return ret;
}

pub fn initDiags() void {
    const no = tp.FileMask[@intFromEnum(tp.File.FileA)]
        .o(tp.FileMask[@intFromEnum(tp.File.FileH)])
        .o(tp.RankMask[@intFromEnum(tp.Rank.Rank1)])
        .o(tp.RankMask[@intFromEnum(tp.Rank.Rank8)]);

    for (0..64) |sq| {
        const square: tp.Square = @enumFromInt(sq);
        const mask = tp.DiagonalMask[square.diagonal()]
            .o(tp.AntiDiagonalMask[square.antiDiagonal()])
            .without(square.toBoard())
            .without(no);

        DiagShifts[sq] = 64 - mask.popcount();
        DiagMasks[sq] = mask;

        for (0..(1 << 6)) |dia| {
            for (0..(1 << 6)) |ant| {
                const diag = dia << 1;
                const anti = ant << 1;

                const pie: tp.BitBoard = .{ .v = toDiagonal(diag, square.diagonal()) |
                    toAntiDiagonal(anti, square.antiDiagonal()) };

                if (pie.a(mask).v != pie.v) continue;
                if (pie.check(square)) continue;

                const index = (pie.v *% DiagMagics[sq].v) >> @intCast(DiagShifts[sq]);
                DiagAttacks[sq][index] = getDiagAttacks(square, pie);
            }
        }
    }
}

pub inline fn getDiagMask(s: tp.Square) tp.BitBoard {
    return DiagMasks[@intFromEnum(s)];
}

pub inline fn getDiag(s: tp.Square, bl: tp.BitBoard) tp.BitBoard {
    const shift = DiagShifts[@intFromEnum(s)];
    const magic = DiagMagics[@intFromEnum(s)];
    const index = (bl.v *% magic.v) >> @intCast(shift);
    return DiagAttacks[@intFromEnum(s)][index];
}
