const std = @import("std");

pub const Direction = enum(u6) {
    North = 8,
    South = -8,
    East = 1,
    West = -1,
    NorthEast = 9,
    NorthWest = 7,
    SouthEast = -7,
    SouthWest = -9,
};

pub const Rank = enum(u6) {
    Rank1,
    Rank2,
    Rank3,
    Rank4,
    Rank5,
    Rank6,
    Rank7,
    Rank8,
};

pub const File = enum(u6) {
    FileA,
    FileB,
    FileC,
    FileD,
    FileE,
    FileF,
    FileG,
    FileH,
};

pub const Square = enum(u6) {
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

    pub inline fn new(r: Rank, f: File) Square {
        return @enumFromInt(@intFromEnum(r) * 8 + @intFromEnum(f));
    }

    pub inline fn rank(self: Square) Rank {
        return @enumFromInt(@intFromEnum(self) >> 3);
    }

    pub inline fn file(self: Square) File {
        return @enumFromInt(@intFromEnum(self) & 0b111);
    }

    pub inline fn diagonal(self: Square) u6 {
        return @intFromEnum(self.rank()) + @intFromEnum(self.file());
    }

    pub inline fn anti_diagonal(self: Square) u6 {
        return @intFromEnum(self.rank()) + 7 - @intFromEnum(self.file());
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
        self.* = @enumFromInt(@intFromEnum(self) + @intFromEnum(dir));
        return self;
    }

    pub fn to_board(self: Square) BitBoard {
        return .{ .v = @as(u64, 0b1) << @intFromEnum(self) };
    }
};

pub const PieceType = enum(u6) { Pawn, Knight, Bishop, Rook, Queen, King };

pub const BitBoard = packed struct {
    v: u64,

    pub fn new() BitBoard {
        return .{ .v = 0 };
    }

    pub inline fn check(self: BitBoard, s: Square) bool {
        return self.v & s.to_board().v != 0;
    }

    pub inline fn set(self: *BitBoard, s: Square) *BitBoard {
        self.v = self.v | s.to_board().v;
        return self;
    }

    pub inline fn unset(self: *BitBoard, s: Square) *BitBoard {
        self.v = self.v | ~s.to_board().v;
        return self;
    }

    pub inline fn lsb(self: BitBoard) ?Square {
        return switch (@ctz(self.v)) {
            @bitSizeOf(u64) => null,
            else => |x| @as(Square, @enumFromInt(x)),
        };
    }

    pub fn pop_lsb(self: *BitBoard) ?Square {
        if (lsb(self.*)) |ret| {
            self.unset(ret);
            return ret;
        }

        return null;
    }

    //From lc0
    pub fn mirror(self: *BitBoard) *BitBoard {
        self.v = (self.v & 0x00000000FFFFFFFF) << 32 | (self.v & 0xFFFFFFFF00000000) >> 32;
        self.v = (self.v & 0x0000FFFF0000FFFF) << 16 | (self.v & 0xFFFF0000FFFF0000) >> 16;
        self.v = (self.v & 0x00FF00FF00FF00FF) << 8 | (self.v & 0xFF00FF00FF00FF00) >> 8;
        return self;
    }

    pub fn mirror_h(self: *BitBoard) *BitBoard {
        self.v = (self.v & 0x0F0F0F0F0F0F0F0F) << 4 | (self.v & 0xF0F0F0F0F0F0F0F0) >> 4;
        self.v = (self.v & 0x3333333333333333) << 2 | (self.v & 0xCCCCCCCCCCCCCCCC) >> 2;
        self.v = (self.v & 0x5555555555555555) << 1 | (self.v & 0xAAAAAAAAAAAAAAAA) >> 1;
        return self;
    }

    pub fn print(b: *const BitBoard, wr: anytype) !void {
        try wr.print("  a b c d e f g h\n", .{});
        for (0..8) |in| {
            for (0..8) |j| {
                const i = 7 - in;

                if(j == 0) try wr.print("{} ", .{i + 1});
                if (b.check(Square.new(@enumFromInt(i), @enumFromInt(j))))
                    try wr.print("x", .{})
                else
                    try wr.print(" ", .{});
                if (j == 7) try wr.print("\n", .{}) else try wr.print(" ", .{});
            }
        }
    }
};
