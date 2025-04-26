const std = @import("std");
const tp = @import("types.zig");

pub const Mirror = packed struct {
    white: bool,
    horiz: bool,
};

pub const Board = struct {
    o_pieces: tp.BitBoard,
    t_pieces: tp.BitBoard,
    pawns: tp.BitBoard,
    diags: tp.BitBoard,
    lines: tp.BitBoard,
    o_king: tp.Square,
    t_king: tp.Square,

    castle: tp.Castling,
    mir: Mirror,

    history: [2048]u64,
    history_index: u12,
    move_rule: u8, //This is given in plies, not moves

    fn scramble(i: u64) u64 {
        return 0xfad0d7f2fbb059f1 *% (i +% 0xbaad41cdcb839961) +%
        0x7acec0050bf82f43 *% ((i >> 31) +% 0xd571b3a92b1b2755);
    }

    pub inline fn hash(self: *const Board) u64 {
        var ret = scramble(self.o_pieces.v);
        ret *%= scramble(self.t_pieces.v);
        ret ^= scramble(self.pawns.v);
        ret +%= scramble(self.diags.v);
        ret *%= scramble(self.lines.v);
        ret ^= scramble(self.o_king.toBoard().op_or(self.t_king.toBoard()).v);

        const castle: u4 = @bitCast(self.castle);
        const shift = (@as(i32, castle) & 0b0011) * 4 - (@as(i32, castle) & 0b1100);
        if (shift > 0) ret >>= @intCast(shift) else ret <<= @intCast(-shift);

        return ret;
    }

    pub fn mirror(self: *Board) *Board {
        self.mir.white = !self.mir.white;
        _ = self.castle.mirror();

        self.o_pieces.v ^= self.t_pieces.v;
        self.t_pieces.v ^= self.o_pieces.v;
        self.o_pieces.v ^= self.t_pieces.v;

        _ = self.o_pieces.mirror();
        _ = self.t_pieces.mirror();
        _ = self.pawns.mirror();
        _ = self.diags.mirror();
        _ = self.lines.mirror();
        _ = self.o_king.mirror();
        _ = self.t_king.mirror();

        const king = self.o_king;
        self.o_king = self.t_king;
        self.t_king = king;

        return self;
    }

    pub fn mirrorH(self: *Board) *Board {
        self.mir.horiz = !self.mir.horiz;

        _ = self.o_pieces.mirrorH();
        _ = self.t_pieces.mirrorH();
        _ = self.pawns.mirrorH();
        _ = self.diags.mirrorH();
        _ = self.lines.mirrorH();
        _ = self.o_king.mirrorH();
        _ = self.t_king.mirrorH();

        return self;
    }

    pub fn check(b: *const Board, s: tp.Square) ?struct { our: bool, typ: tp.PieceType } {
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

        return .{ .our = col, .typ = .Knight };
    }

    pub inline fn enPassant(self: *const Board) tp.BitBoard {
        return .{ .v = self.pawns.v & ~(self.o_pieces.v | self.t_pieces.v) };
    }

    const FenStage = enum { Start, Who, Castling, EnPassant };
    pub fn fromFen(fen: []const u8) !Board {
        var stage = FenStage.Start;
        var ret = Board{
            .o_pieces = tp.BitBoard.new(),
            .t_pieces = tp.BitBoard.new(),
            .pawns = tp.BitBoard.new(),
            .diags = tp.BitBoard.new(),
            .lines = tp.BitBoard.new(),
            .o_king = .a1,
            .t_king = .a1,

            .castle = .{ .ok = false, .oq = false, .tk = false, .tq = false },
            .mir = .{ .white = true, .horiz = false },

            .history = std.mem.zeroes([2048]u64),
            .history_index = 0,
            .move_rule = 0,
        };

        var first_space = false;
        var en_passant_char: u8 = 0;
        var pos = tp.Square.a8;
        var contin = false;
        for (fen) |c| {
            switch (stage) {
                .Start => {
                    switch (c) {
                        '/' => {
                            if (pos.file() == .FileH) {
                                pos = @enumFromInt(@intFromEnum(pos) - 15);
                            } else {
                                return error.InvalidNewlineSlash;
                            }
                        },
                        '1'...'8' => {
                            const add: tp.Square =
                                @enumFromInt(@intFromEnum(pos) + @as(u8, c - '1'));
                            pos = if (add.file() == .FileH)
                                add
                            else
                                @enumFromInt(@intFromEnum(add) + 1);

                            if (add.file() == .FileH and pos == .h1) contin = true;
                        },
                        else => {
                            _ = switch (c) {
                                'P', 'p' => ret.pawns.set(pos),
                                'B', 'b' => ret.diags.set(pos),
                                'R', 'r' => ret.lines.set(pos),
                                'Q', 'q' => {
                                    _ = ret.diags.set(pos);
                                    _ = ret.lines.set(pos);
                                },
                                'N', 'n' => {},
                                'K' => ret.o_king = pos,
                                'k' => ret.t_king = pos,
                                else => return error.InvalidPiece,
                            };
                            _ = switch (c) {
                                'P', 'N', 'B', 'R', 'Q', 'K' => ret.o_pieces.set(pos),
                                'p', 'n', 'b', 'r', 'q', 'k' => ret.t_pieces.set(pos),
                                else => return error.InvalidPiece,
                            };

                            if (pos == .h1) {
                                first_space = false;
                                stage = .Who;
                            }

                            if (pos.file() != .FileH)
                                pos = @enumFromInt(@intFromEnum(pos) + 1);
                        },
                    }

                    if (pos == .h1) {
                        if (contin) {
                            first_space = false;
                            stage = .Who;
                        } else {
                            contin = true;
                        }
                    }
                },
                .Who => {
                    switch (c) {
                        'w' => {},
                        'b' => _ = ret.mirror().mirrorH(),
                        ' ' => {
                            if (first_space) {
                                first_space = false;
                                stage = .Castling;
                            } else first_space = true;
                        },
                        else => return error.InvalidSide,
                    }
                },
                .Castling => {
                    switch (c) {
                        '-' => continue,
                        'K' => ret.castle.ok = true,
                        'Q' => ret.castle.oq = true,
                        'k' => ret.castle.tk = true,
                        'q' => ret.castle.tq = true,
                        ' ' => {
                            if (!ret.mir.white)
                                _ = ret.castle.mirror();
                            stage = .EnPassant;
                        },
                        else => return error.InvalidCastle,
                    }
                },
                .EnPassant => {
                    if (en_passant_char == 0) {
                        en_passant_char = c;
                        if (en_passant_char == '-') break else continue;
                    }

                    switch (c) {
                        ' ' => break,
                        '1'...'8' => {
                            if ('a' > en_passant_char or en_passant_char > 'h')
                                return error.InvalidEnPassant;
                            const file: tp.File = @enumFromInt(en_passant_char - 'a');
                            const rank: tp.Rank = @enumFromInt(c - '1');
                            var sq = tp.Square.new(rank, file);
                            if (!ret.mir.white)
                                _ = sq.mirror().mirrorH();

                            _ = ret.pawns.set(sq);

                            break;
                        },
                        else => return error.InvalidEnPassant,
                    }
                },
            }
        }

        return ret;
    }

    pub fn apply(self: *Board, m: tp.Move) !tp.Remove {
        self.history[self.history_index] = self.hash();
        self.history_index += 1;

        const pas = self.enPassant().lsb();
        const cas = self.castle;
        const move_rule = self.move_rule;
        self.move_rule += 1;

        self.pawns.v &= self.o_pieces.op_or(self.t_pieces).v; //Unsetting en passant

        if (self.pawns.check(m.from)) self.move_rule = 0;

        var ret: ?tp.PieceType = null;
        switch (m.typ) {
            .Normal => {
                _ = self.o_pieces.move(m.from, m.to);
                const th = self.t_pieces.checkUnset(m.to);
                const pa = self.pawns.move(m.from, m.to);
                const di = self.diags.move(m.from, m.to);
                const li = self.lines.move(m.from, m.to);
                if (pa == .Moved) {
                    if (m.from.getApplySafe(.NorthNorth)) |fr| {
                        if (m.to == fr)
                            _ = self.pawns.set(m.from.getApply(.North));
                    }
                } else if ((li == .Moved or li == .Both) and di != .Moved) {
                    if (m.from == .a1) {
                        if (self.mir.horiz)
                            self.castle.ok = false
                        else
                            self.castle.oq = false;
                    } else if (m.from == .h1) {
                        if (self.mir.horiz)
                            self.castle.oq = false
                        else
                            self.castle.ok = false;
                    }
                } else if (self.o_king == m.from) {
                    self.o_king = m.to;
                    self.castle.ok = false;
                    self.castle.oq = false;
                }
                if ((li == .Removed or li == .Both) and di != .Removed and di != .Both) {
                    if (m.to == .a8) {
                        if (self.mir.horiz)
                            self.castle.tk = false
                        else
                            self.castle.tq = false;
                    } else if (m.to == .h8) {
                        if (self.mir.horiz)
                            self.castle.tq = false
                        else
                            self.castle.tk = false;
                    }
                }

                if (th == .Removed or th == .Both) {
                    if (pa == .Removed or pa == .Both) {
                        ret = .Pawn;
                    } else if (di == .Removed or di == .Both) {
                        ret = if (li == .Removed or li == .Both) .Queen else .Bishop;
                    } else if (li == .Removed or li == .Both) {
                        ret = .Rook;
                    } else {
                        ret = .Knight;
                    }
                }
            },
            .EnPassant => {
                _ = self.o_pieces.move(m.from, m.to);

                const hit = m.to.getApply(.South);
                _ = self.t_pieces.unset(hit);
                _ = self.pawns.unset(hit).move(m.from, m.to);
            },
            .CastleKingside => {
                _ = self.o_pieces.move(m.from, m.to);
                self.o_king = m.to;

                const fr: tp.Square = if (self.mir.horiz) .a1 else .h1;
                const to: tp.Square = if (self.mir.horiz) .c1 else .f1;
                _ = self.o_pieces.move(fr, to);
                _ = self.lines.move(fr, to);

                self.castle.ok = false;
                self.castle.oq = false;
            },
            .CastleQueenside => {
                _ = self.o_pieces.move(m.from, m.to);
                self.o_king = m.to;

                const fr: tp.Square = if (self.mir.horiz) .h1 else .a1;
                const to: tp.Square = if (self.mir.horiz) .e1 else .d1;
                _ = self.o_pieces.move(fr, to);
                _ = self.lines.move(fr, to);

                self.castle.ok = false;
                self.castle.oq = false;
            },
            .PromKnight => {
                const th = self.t_pieces.checkUnset(m.to);
                const di = self.diags.checkUnset(m.to);
                const li = self.lines.checkUnset(m.to);
                _ = self.o_pieces.move(m.from, m.to);
                _ = self.pawns.unset(m.from);
                if (th == .Removed) {
                    if (di == .Removed) {
                        ret = if (li == .Removed) .Queen else .Bishop;
                    } else if (li == .Removed) {
                        ret = .Rook;
                    } else {
                        ret = .Knight;
                    }
                }
            },
            .PromBishop => {
                const th = self.t_pieces.checkUnset(m.to);
                const di = self.diags.checkUnset(m.to);
                const li = self.lines.checkUnset(m.to);
                _ = self.o_pieces.move(m.from, m.to);
                _ = self.pawns.unset(m.from);
                _ = self.diags.set(m.to);
                if (th == .Removed) {
                    if (di == .Removed) {
                        ret = if (li == .Removed) .Queen else .Bishop;
                    } else if (li == .Removed) {
                        ret = .Rook;
                    } else {
                        ret = .Knight;
                    }
                }
            },
            .PromRook => {
                const th = self.t_pieces.checkUnset(m.to);
                const di = self.diags.checkUnset(m.to);
                const li = self.lines.checkUnset(m.to);
                _ = self.o_pieces.move(m.from, m.to);
                _ = self.pawns.unset(m.from);
                _ = self.lines.set(m.to);
                if (th == .Removed) {
                    if (di == .Removed) {
                        ret = if (li == .Removed) .Queen else .Bishop;
                    } else if (li == .Removed) {
                        ret = .Rook;
                    } else {
                        ret = .Knight;
                    }
                }
            },
            .PromQueen => {
                const th = self.t_pieces.checkUnset(m.to);
                const di = self.diags.checkUnset(m.to);
                const li = self.lines.checkUnset(m.to);
                _ = self.o_pieces.move(m.from, m.to);
                _ = self.pawns.unset(m.from);
                _ = self.diags.set(m.to);
                _ = self.lines.set(m.to);
                if (th == .Removed) {
                    if (di == .Removed) {
                        ret = if (li == .Removed) .Queen else .Bishop;
                    } else if (li == .Removed) {
                        ret = .Rook;
                    } else {
                        ret = .Knight;
                    }
                }
            },
        }

        if (ret != null) self.move_rule = 0;

        _ = self.mirror();
        return .{ .typ = ret, .pas = pas, .cas = cas, .move_rule = move_rule };
    }

    pub fn remove(self: *Board, m: tp.Move, u: tp.Remove) !void {
        self.history_index -= 1;

        _ = self.mirror();
        self.pawns.v &= self.o_pieces.op_or(self.t_pieces).v; //Unsetting old en passant

        switch (m.typ) {
            .Normal => {
                _ = self.o_pieces.move(m.to, m.from);
                _ = self.pawns.move(m.to, m.from);
                _ = self.diags.move(m.to, m.from);
                _ = self.lines.move(m.to, m.from);
                if (self.o_king == m.to) {
                    self.o_king = m.from;
                }
            },
            .EnPassant => {
                _ = self.o_pieces.move(m.to, m.from);

                const hit = m.to.getApply(.South);
                _ = self.t_pieces.set(hit);
                _ = self.pawns.set(hit).move(m.to, m.from);
            },
            .CastleKingside => {
                _ = self.o_pieces.move(m.to, m.from);
                self.o_king = m.from;

                const fr: tp.Square = if (self.mir.horiz) .a1 else .h1;
                const to: tp.Square = if (self.mir.horiz) .c1 else .f1;
                _ = self.o_pieces.move(to, fr);
                _ = self.lines.move(to, fr);
            },
            .CastleQueenside => {
                _ = self.o_pieces.move(m.to, m.from);
                self.o_king = m.from;

                const fr: tp.Square = if (self.mir.horiz) .h1 else .a1;
                const to: tp.Square = if (self.mir.horiz) .e1 else .d1;
                _ = self.o_pieces.move(to, fr);
                _ = self.lines.move(to, fr);
            },
            .PromKnight => {
                _ = self.o_pieces.move(m.to, m.from);
                _ = self.pawns.set(m.from);
            },
            .PromBishop => {
                _ = self.o_pieces.move(m.to, m.from);
                _ = self.pawns.set(m.from);
                _ = self.diags.unset(m.to);
            },
            .PromRook => {
                _ = self.o_pieces.move(m.to, m.from);
                _ = self.pawns.set(m.from);
                _ = self.lines.unset(m.to);
            },
            .PromQueen => {
                _ = self.o_pieces.move(m.to, m.from);
                _ = self.pawns.set(m.from);
                _ = self.diags.unset(m.to);
                _ = self.lines.unset(m.to);
            },
        }

        if (u.typ) |typ| {
            _ = self.t_pieces.set(m.to);
            switch (typ) {
                .Pawn => _ = self.pawns.set(m.to),
                .Knight => {},
                .Bishop => _ = self.diags.set(m.to),
                .Rook => _ = self.lines.set(m.to),
                .Queen => {
                    _ = self.diags.set(m.to);
                    _ = self.lines.set(m.to);
                },
                else => return error.InvalidRemoveTyp,
            }
        }

        self.move_rule = u.move_rule;
        self.castle = u.cas;
        if (u.pas) |pas| {
            self.pawns.v |= pas.toBoard().v; //Resetting en passant
        }
    }

    pub fn print(b: *const Board) void {
        std.debug.print("  a b c d e f g h\n", .{});
        for (0..8) |in| {
            for (0..8) |j| {
                const i = 7 - in;

                if (j == 0) std.debug.print("{} ", .{i + 1});
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
                    else switch (che.typ) {
                        .Pawn => "p",
                        .Knight => "n",
                        .Bishop => "b",
                        .Rook => "r",
                        .Queen => "q",
                        .King => "k",
                    };

                    std.debug.print("{s}", .{pi});
                } else std.debug.print(" ", .{});

                if (j == 7) std.debug.print("\n", .{}) else std.debug.print(" ", .{});
            }
        }
    }
};
