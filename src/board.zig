const std = @import("std");
const tp = @import("types.zig");

pub const Board = struct {
    o_pieces: tp.BitBoard,
    t_pieces: tp.BitBoard,
    pawns: tp.BitBoard,
    diags: tp.BitBoard,
    lines: tp.BitBoard,
    o_king: tp.Square,
    t_king: tp.Square,

    white: bool,
    mirrored_h: bool,

    pub fn mirror(self: *Board) *Board {
        self.white = !self.white;

        _ = self.o_pieces.mirror();
        _ = self.t_pieces.mirror();
        _ = self.pawns.mirror();
        _ = self.diags.mirror();
        _ = self.lines.mirror();
        _ = self.o_king.mirror();
        _ = self.t_king.mirror();

        return self;
    }

    pub fn mirror_h(self: *Board) *Board {
        self.mirrored_h = !self.mirrored_h;

        _ = self.o_pieces.mirror_h();
        _ = self.t_pieces.mirror_h();
        _ = self.pawns.mirror_h();
        _ = self.diags.mirror_h();
        _ = self.lines.mirror_h();
        _ = self.o_king.mirror_h();
        _ = self.t_king.mirror_h();

        return self;
    }

    pub fn check(b: Board, s: tp.Square) ?struct { our: bool, typ: tp.PieceType } {
        const col =
            if (b.o_pieces.check(s)) true else if (b.t_pieces.check(s)) false else return null;

        if (b.pawns.check(s))
            return .{ .our = col, .typ = .Pawn };

        if (b.diags.check(s)) {
            if (b.lines.check(s))
                return .{ .our = col, .typ = .Queen };

            return .{ .our = col, .typ = .Bishop };
        }

        if (b.lines.check(s))
            return .{ .our = col, .typ = .Rook };

        if (b.o_king == s or b.t_king == s)
            return .{ .our = col, .typ = .King };

        return null;
    }

    const FenStage = enum { Start, Who, Castling, EnPassant };
    pub fn from_fen(fen: []const u8) !Board {
        var stage = FenStage.Start;
        var ret = Board{
            .o_pieces = tp.BitBoard.new(),
            .t_pieces = tp.BitBoard.new(),
            .pawns = tp.BitBoard.new(),
            .diags = tp.BitBoard.new(),
            .lines = tp.BitBoard.new(),
            .o_king = .a1,
            .t_king = .a1,

            .white = true,
            .mirrored_h = false,
        };

        var pos = tp.Square.a8;
        for (fen) |c| {
            switch (stage) {
                .Start => {
                    switch (c) {
                        '/' => {
                            if (pos.file() == .FileH)
                                pos = @enumFromInt(@intFromEnum(pos) - 15)
                            else
                                return error.InvalidNewlineSlash;
                        },
                        '1'...'8' => {
                            pos = @enumFromInt(@intFromEnum(pos) + @as(u8, c - '0'));
                        },
                        else => {
                            _ = switch (c) {
                                'P' | 'p' => ret.pawns.set(pos),
                                'B' | 'b' => ret.diags.set(pos),
                                'R' | 'r' => ret.lines.set(pos),
                                'Q' | 'q' => {
                                    _ = ret.diags.set(pos);
                                    _ = ret.lines.set(pos);
                                },
                                'N' | 'n' => {},
                                'K' => ret.o_king = pos,
                                'k' => ret.t_king = pos,
                                else => return error.InvalidPiece,
                            };
                            _ = switch (c) {
                                'P' | 'N' | 'B' | 'R' | 'Q' | 'K' => ret.o_pieces.set(pos),
                                'p' | 'n' | 'b' | 'r' | 'q' | 'k' => ret.t_pieces.set(pos),
                                else => return error.InvalidPiece,
                            };

                            pos = @enumFromInt(@intFromEnum(pos) + 1);
                        },
                    }

                    if (pos == .a8)
                        stage = .Who;
                },
                .Who => return error.TODO,
                .Castling => return error.TODO,
                .EnPassant => return error.TODO,
            }
        }

        return ret;
    }

    pub fn print(b: *const Board, wr: anytype) !void {
        try wr.print("  a b c d e f g h\n", .{});
        for (0..8) |in| {
            for (0..8) |j| {
                const i = 7 - in;

                if (j == 0) try wr.print("{} ", .{i + 1});
                const ch = b.check(tp.Square.new(@enumFromInt(i), @enumFromInt(j)));
                if (ch) |che| {
                    const pi = if (che.our)
                        switch (che.typ) {
                            .Pawn => "P",
                            .Knight => "N",
                            .Bishop => "B",
                            .Rook => "R",
                            .Queen => "Q",
                            .King => "K",
                        }
                    else
                        switch (che.typ) {
                            .Pawn => "p",
                            .Knight => "n",
                            .Bishop => "b",
                            .Rook => "r",
                            .Queen => "q",
                            .King => "k",
                        };

                    try wr.print("{s}", .{pi});
                } else try wr.print(" ", .{});

                if (j == 7) try wr.print("\n", .{}) else try wr.print(" ", .{});
            }
        }
    }
};
