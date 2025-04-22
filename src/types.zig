const std = @import("std");

pub const Direction = enum(i6) {
    North = 8,
    South = -8,
    East = 1,
    West = -1,
    NorthEast = 9,
    NorthWest = 7,
    SouthEast = -7,
    SouthWest = -9,
    NorthNorth = 16,
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

pub const RankMask = [_]BitBoard{
    .{ .v = 0xFF },
    .{ .v = 0xFF00 },
    .{ .v = 0xFF0000 },
    .{ .v = 0xFF000000 },
    .{ .v = 0xFF00000000 },
    .{ .v = 0xFF0000000000 },
    .{ .v = 0xFF000000000000 },
    .{ .v = 0xFF00000000000000 },
};

pub const FileMask = [_]BitBoard{
    .{ .v = 0x0101010101010101 },
    .{ .v = 0x0202020202020202 },
    .{ .v = 0x0404040404040404 },
    .{ .v = 0x0808080808080808 },
    .{ .v = 0x1010101010101010 },
    .{ .v = 0x2020202020202020 },
    .{ .v = 0x4040404040404040 },
    .{ .v = 0x8080808080808080 },
};

pub const DiagonalMask = [_]BitBoard{
    .{ .v = 0x80 },
    .{ .v = 0x8040 },
    .{ .v = 0x804020 },
    .{ .v = 0x80402010 },
    .{ .v = 0x8040201008 },
    .{ .v = 0x804020100804 },
    .{ .v = 0x80402010080402 },
    .{ .v = 0x8040201008040201 },
    .{ .v = 0x4020100804020100 },
    .{ .v = 0x2010080402010000 },
    .{ .v = 0x1008040201000000 },
    .{ .v = 0x804020100000000 },
    .{ .v = 0x402010000000000 },
    .{ .v = 0x201000000000000 },
    .{ .v = 0x100000000000000 },
};

pub const AntiDiagonalMask = [_]BitBoard{
    .{ .v = 0x1 },
    .{ .v = 0x102 },
    .{ .v = 0x10204 },
    .{ .v = 0x1020408 },
    .{ .v = 0x102040810 },
    .{ .v = 0x10204081020 },
    .{ .v = 0x1020408102040 },
    .{ .v = 0x102040810204080 },
    .{ .v = 0x204081020408000 },
    .{ .v = 0x408102040800000 },
    .{ .v = 0x810204080000000 },
    .{ .v = 0x1020408000000000 },
    .{ .v = 0x2040800000000000 },
    .{ .v = 0x4080000000000000 },
    .{ .v = 0x8000000000000000 },
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
        return @intFromEnum(self.rank()) + 7 - @intFromEnum(self.file());
    }

    pub inline fn antiDiagonal(self: Square) u6 {
        return @intFromEnum(self.rank()) + @intFromEnum(self.file());
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
        self.* = @enumFromInt(@as(i7, @intFromEnum(self)) + @intFromEnum(dir));
        return self;
    }

    pub inline fn getApply(self: Square, dir: Direction) Square {
        return @enumFromInt(@as(i7, @intFromEnum(self)) + @intFromEnum(dir));
    }

    pub inline fn toBoard(self: Square) BitBoard {
        return .{ .v = @as(u64, 0b1) << @intFromEnum(self) };
    }

    pub fn mirror(self: *Square) *Square {
        self.* = @enumFromInt(@intFromEnum(self.*) ^ 0b111000);
        return self;
    }

    pub fn mirrorH(self: *Square) *Square {
        self.* = @enumFromInt(@intFromEnum(self.*) ^ 0b000111);
        return self;
    }

    pub fn toString(self: Square) [2]u8 {
        const fil = self.file();
        const ran = self.rank();
        var ret: [2]u8 = undefined;

        ret[0] = switch (fil) {
            .FileA => 'a',
            .FileB => 'b',
            .FileC => 'c',
            .FileD => 'd',
            .FileE => 'e',
            .FileF => 'f',
            .FileG => 'g',
            .FileH => 'h',
        };

        ret[1] = switch (ran) {
            .Rank1 => '1',
            .Rank2 => '2',
            .Rank3 => '3',
            .Rank4 => '4',
            .Rank5 => '5',
            .Rank6 => '6',
            .Rank7 => '7',
            .Rank8 => '8',
        };

        return ret;
    }
};

pub const PieceType = enum(u6) { Pawn, Knight, Bishop, Rook, Queen, King };

pub const BitBoard = packed struct {
    v: u64,

    pub inline fn new() BitBoard {
        return .{ .v = 0 };
    }

    pub inline fn a(self: BitBoard, oth: BitBoard) BitBoard {
        return .{ .v = self.v & oth.v };
    }

    pub inline fn o(self: BitBoard, oth: BitBoard) BitBoard {
        return .{ .v = self.v | oth.v };
    }

    pub inline fn n(self: BitBoard) BitBoard {
        return .{ .v = ~self.v };
    }

    pub inline fn check(self: BitBoard, s: Square) bool {
        return self.v & s.toBoard().v != 0;
    }

    pub inline fn set(self: *BitBoard, s: Square) *BitBoard {
        self.v = self.v | s.toBoard().v;
        return self;
    }

    pub inline fn unset(self: *BitBoard, s: Square) *BitBoard {
        self.v = self.v & ~s.toBoard().v;
        return self;
    }

    pub inline fn lsb(self: BitBoard) ?Square {
        return switch (@ctz(self.v)) {
            @bitSizeOf(u64) => null,
            else => |x| @as(Square, @enumFromInt(x)),
        };
    }

    pub fn popLsb(self: *BitBoard) ?Square {
        if (lsb(self.*)) |ret| {
            _ = self.unset(ret);
            return ret;
        }

        return null;
    }

    pub inline fn popcount(self: BitBoard) usize {
        return @as(usize, @popCount(self.v));
    }

    //From lc0
    pub fn mirror(self: *BitBoard) *BitBoard {
        self.v = (self.v & 0x00000000FFFFFFFF) << 32 | (self.v & 0xFFFFFFFF00000000) >> 32;
        self.v = (self.v & 0x0000FFFF0000FFFF) << 16 | (self.v & 0xFFFF0000FFFF0000) >> 16;
        self.v = (self.v & 0x00FF00FF00FF00FF) << 8 | (self.v & 0xFF00FF00FF00FF00) >> 8;
        return self;
    }

    pub fn mirrorH(self: *BitBoard) *BitBoard {
        self.v = (self.v & 0x0F0F0F0F0F0F0F0F) << 4 | (self.v & 0xF0F0F0F0F0F0F0F0) >> 4;
        self.v = (self.v & 0x3333333333333333) << 2 | (self.v & 0xCCCCCCCCCCCCCCCC) >> 2;
        self.v = (self.v & 0x5555555555555555) << 1 | (self.v & 0xAAAAAAAAAAAAAAAA) >> 1;
        return self;
    }

    pub const CheckType = enum {
        Nothing,
        Moved,
        Removed,
    };
    pub inline fn move(self: *BitBoard, fr: Square, to: Square) CheckType {
        if (self.check(fr)) {
            _ = self.unset(fr).set(to);
            return .Moved;
        }

        if (self.check(to)) {
            _ = self.unset(to);
            return .Removed;
        }

        return .Nothing;
    }

    pub inline fn checkUnset(self: *BitBoard, sq: Square) CheckType {
        if (self.check(sq)) {
            _ = self.unset(sq);
            return .Removed;
        }

        return .Nothing;
    }

    pub inline fn without(self: BitBoard, oth: BitBoard) BitBoard {
        return .{ .v = self.v & ~oth.v };
    }

    pub fn print(b: *const BitBoard) void {
        std.debug.print("  a b c d e f g h\n", .{});
        for (0..8) |in| {
            for (0..8) |j| {
                const i = 7 - in;

                if (j == 0) std.debug.print("{} ", .{i + 1});
                if (b.check(Square.new(@enumFromInt(i), @enumFromInt(j))))
                    std.debug.print("x", .{})
                else
                    std.debug.print(" ", .{});
                if (j == 7) std.debug.print("\n", .{}) else std.debug.print(" ", .{});
            }
        }
    }
};

pub const Castling = packed struct {
    ok: bool,
    oq: bool,
    tk: bool,
    tq: bool,

    pub inline fn mirror(self: *Castling) *Castling {
        const us: u4 = @bitCast(self.*);
        self.* = @bitCast((us & 0b0011) << 2 | (us & 0b1100) >> 2);

        return self;
    }
};

pub const MoveType = enum(u3) {
    Normal,
    EnPassant,
    CastleKingside,
    CastleQueenside,
    PromKnight,
    PromBishop,
    PromRook,
    PromQueen,
};
pub const Move = packed struct {
    from: Square,
    to: Square,
    typ: MoveType,

    pub fn print(self: Move) void {
        switch (self.typ) {
            .Normal => std.debug.print("{s}{s}", .{self.from.toString(), self.to.toString()}),
            .EnPassant => std.debug.print("{s}x", .{self.from.toString()}),
            .CastleKingside => std.debug.print("O-O", .{}),
            .CastleQueenside => std.debug.print("O-O-O", .{}),
            .PromKnight => std.debug.print("{s}N{s}",
                .{self.from.toString(), self.to.toString()}),
            .PromBishop => std.debug.print("{s}B{s}",
                .{self.from.toString(), self.to.toString()}),
            .PromRook => std.debug.print("{s}R{s}",
                .{self.from.toString(), self.to.toString()}),
            .PromQueen => std.debug.print("{s}Q{s}",
                .{self.from.toString(), self.to.toString()}),
        }
    }
};

pub const Remove = struct {
    typ: ?PieceType,
    pas: ?Square,
    cas: Castling,
};
