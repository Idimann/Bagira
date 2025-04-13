const std = @import("std");

pub const Direction = enum(u8) {
    North = 8,
    South = -8,
    East = 1,
    West = -1,
    NorthEast = 9,
    NorthWest = 7,
    SouthEast = -7,
    SouthWest = -9,
};

pub const Rank = enum(u8) {
    Rank1,
    Rank2,
    Rank3,
    Rank4,
    Rank5,
    Rank6,
    Rank7,
    Rank8,
};

pub const File = enum(u8) {
    FileA,
    FileB,
    FileC,
    FileD,
    FileE,
    FileF,
    FileG,
    FileH,
};

pub const Square = enum(u8) {
    // zig fmt: off
    a1, b1, c1, d1, e1, f1, g1, h1,
    a2, b2, c2, d2, e2, f2, g2, h2,
    a3, b3, c3, d3, e3, f3, g3, h3,
    a4, b4, c4, d4, e4, f4, g4, h4,
    a5, b5, c5, d5, e5, f5, g5, h5,
    a6, b6, c6, d6, e6, f6, g6, h6,
    a7, b7, c7, d7, e7, f7, g7, h7,
    a8, b8, c8, d8, e8, f8, g8, h8,
    // zig fmt: on

    pub inline fn rank(self: Square) Rank {
        return @enumFromInt(@intFromEnum(self) & 0b111);
    }

    pub inline fn file(self: Square) File {
        return @enumFromInt(@intFromEnum(self) >> 3);
    }

    pub inline fn check(self: Square, d: Direction) bool {
        const r = rank(self);
        const f = file(self);

        switch (d) {
            .North => r < 7,
            .South => r > 0,
            .East => f < 7,
            .West => f > 0,
            .NorthEast => r < 7 and f < 7,
            .NorthWest => r < 7 and f > 0,
            .SouthEast => r > 0 and f < 7,
            .SouthWest => r > 0 and f > 0,
        }
    }

    pub fn apply(self: *Square, dir: Direction) *Square {
        return @enumFromInt(@intFromEnum(self) + @intFromEnum(dir));
    }
};

pub const PieceType = enum(u8) { Pawn, Knight, Bishop, Rook, Queen, King };

pub const BitBoard = u64;
