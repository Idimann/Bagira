const std = @import("std");
const tp = @import("types.zig");
const zbr = @import("zobrist.zig");

pub const Side = enum(u1) { White, Black };
pub const Board = struct {
    w_pieces: tp.BitBoard,
    b_pieces: tp.BitBoard,
    pawns: tp.BitBoard,
    diags: tp.BitBoard,
    lines: tp.BitBoard,
    w_king: tp.Square,
    b_king: tp.Square,

    castle: tp.Castling,
    side: Side,

    hash: [2048]u64,
    hash_in: u12,
    move_rule: u8, //This is given in plies, not moves

    pub fn check(b: *const Board, s: tp.Square) ?struct { white: bool, typ: tp.PieceType } {
        const col =
            if (b.w_pieces.check(s)) true else if (b.b_pieces.check(s)) false else return null;

        if (b.pawns.check(s))
            return .{ .white = col, .typ = .Pawn };

        if (b.diags.check(s)) {
            if (b.lines.check(s))
                return .{ .white = col, .typ = .Queen };

            return .{ .white = col, .typ = .Bishop };
        }

        if (b.lines.check(s))
            return .{ .white = col, .typ = .Rook };

        if (b.w_king == s or b.b_king == s)
            return .{ .white = col, .typ = .King };

        return .{ .white = col, .typ = .Knight };
    }

    pub inline fn pieceType(self: *const Board, s: tp.Square) tp.PieceType {
        if (s == self.w_king or s == self.b_king) return .King;
        if (self.pawns.check(s)) return .Pawn;
        if (self.diags.check(s)) {
            if (self.lines.check(s)) return .Queen;
            return .Bishop;
        }
        if (self.lines.check(s)) return .Rook;

        return .Knight;
    }

    fn initHash(self: *Board) void {
        self.hash[self.hash_in] = 0;
        var iter = self.w_pieces.op_or(self.b_pieces);
        while (iter.popLsb()) |sq| {
            const in = @intFromEnum(sq);
            if (self.check(sq)) |typ| {
                if (typ.white)
                    self.hash[self.hash_in] ^= zbr.ZobristTable[0][in]
                else
                    self.hash[self.hash_in] ^= zbr.ZobristTable[1][in];

                if (typ.typ == .Knight) continue;
                self.hash[self.hash_in] ^= switch (typ.typ) {
                    .Pawn => zbr.ZobristTable[2][in],
                    .Bishop => zbr.ZobristTable[3][in],
                    .Rook => zbr.ZobristTable[4][in],
                    .Queen => zbr.ZobristTable[3][in] ^ zbr.ZobristTable[4][in],
                    .King => zbr.ZobristTable[5][in],
                    else => unreachable,
                };
            }
        }

        if (self.enPassant().lsb()) |sq|
            self.hash[self.hash_in] ^= zbr.ZobristTable[2][@intFromEnum(sq)];

        if (self.castle.wk) self.hash[self.hash_in] ^= zbr.CastleTable[0];
        if (self.castle.wq) self.hash[self.hash_in] ^= zbr.CastleTable[1];
        if (self.castle.bk) self.hash[self.hash_in] ^= zbr.CastleTable[2];
        if (self.castle.bq) self.hash[self.hash_in] ^= zbr.CastleTable[3];

        if (self.side == .Black) self.hash[self.hash_in] ^= zbr.SideHash;
    }

    pub inline fn enPassant(self: *const Board) tp.BitBoard {
        return .{ .v = self.pawns.v & ~(self.w_pieces.v | self.b_pieces.v) };
    }

    const FenStage = enum { Start, Who, Castling, EnPassant };
    pub fn fromFen(fen: []const u8) !Board {
        var stage = FenStage.Start;
        var ret = Board{
            .w_pieces = tp.BitBoard.new(),
            .b_pieces = tp.BitBoard.new(),
            .pawns = tp.BitBoard.new(),
            .diags = tp.BitBoard.new(),
            .lines = tp.BitBoard.new(),
            .w_king = .a1,
            .b_king = .a1,

            .castle = .{ .wk = false, .wq = false, .bk = false, .bq = false },
            .side = .White,

            .hash = std.mem.zeroes([2048]u64),
            .hash_in = 0,
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
                                'K' => ret.w_king = pos,
                                'k' => ret.b_king = pos,
                                else => return error.InvalidPiece,
                            };
                            _ = switch (c) {
                                'P', 'N', 'B', 'R', 'Q', 'K' => ret.w_pieces.set(pos),
                                'p', 'n', 'b', 'r', 'q', 'k' => ret.b_pieces.set(pos),
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
                        'w' => ret.side = .White,
                        'b' => ret.side = .Black,
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
                        'K' => ret.castle.wk = true,
                        'Q' => ret.castle.wq = true,
                        'k' => ret.castle.bk = true,
                        'q' => ret.castle.bq = true,
                        ' ' => stage = .EnPassant,
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

                            const sq = tp.Square.new(rank, file);
                            _ = ret.pawns.set(sq);

                            break;
                        },
                        else => return error.InvalidEnPassant,
                    }
                },
            }
        }

        ret.initHash();
        return ret;
    }

    pub fn apply(self: *Board, m: tp.Move) tp.Remove {
        self.hash_in += 1;
        self.hash[self.hash_in] = self.hash[self.hash_in - 1] ^ zbr.SideHash;

        const pas = self.enPassant().lsb();
        const cas = self.castle;
        const move_rule = self.move_rule;
        self.move_rule += 1;

        self.pawns.v &= self.w_pieces.v | self.b_pieces.v; //Unsetting en passant

        if (self.pawns.check(m.from)) self.move_rule = 0;

        var ret: ?tp.PieceType = null;
        switch (m.typ) {
            .Normal => {
                const white = self.w_pieces.move(m.from, m.to);
                const black = self.b_pieces.move(m.from, m.to);
                const pa = self.pawns.move(m.from, m.to);
                const di = self.diags.move(m.from, m.to);
                const li = self.lines.move(m.from, m.to);

                if (white == .Moved or white == .Both)
                    self.hash[self.hash_in] ^= zbr.ZobristTable[0][@intFromEnum(m.from)];
                if (white == .Moved or white == .Removed)
                    self.hash[self.hash_in] ^= zbr.ZobristTable[0][@intFromEnum(m.to)];
                if (black == .Moved or black == .Both)
                    self.hash[self.hash_in] ^= zbr.ZobristTable[1][@intFromEnum(m.from)];
                if (black == .Moved or black == .Removed)
                    self.hash[self.hash_in] ^= zbr.ZobristTable[1][@intFromEnum(m.to)];
                if (pa == .Moved or pa == .Both)
                    self.hash[self.hash_in] ^= zbr.ZobristTable[2][@intFromEnum(m.from)];
                if (pa == .Moved or pa == .Removed)
                    self.hash[self.hash_in] ^= zbr.ZobristTable[2][@intFromEnum(m.to)];
                if (di == .Moved or di == .Both)
                    self.hash[self.hash_in] ^= zbr.ZobristTable[3][@intFromEnum(m.from)];
                if (di == .Moved or di == .Removed)
                    self.hash[self.hash_in] ^= zbr.ZobristTable[3][@intFromEnum(m.to)];
                if (li == .Moved or li == .Both)
                    self.hash[self.hash_in] ^= zbr.ZobristTable[4][@intFromEnum(m.from)];
                if (li == .Moved or li == .Removed)
                    self.hash[self.hash_in] ^= zbr.ZobristTable[4][@intFromEnum(m.to)];

                if (pa == .Moved) {
                    if (self.side == .White) {
                        if (m.from.getApplySafe(.NorthNorth)) |fr| {
                            if (m.to == fr) {
                                const set = m.from.getApply(.North);
                                _ = self.pawns.set(set);
                                self.hash[self.hash_in] ^=
                                    zbr.ZobristTable[2][@intFromEnum(set)];
                            }
                        }
                    } else if (m.from.getApplySafe(.SouthSouth)) |fr| {
                        if (m.to == fr) {
                            const set = m.from.getApply(.South);
                            _ = self.pawns.set(set);
                            self.hash[self.hash_in] ^= zbr.ZobristTable[2][@intFromEnum(set)];
                        }
                    }
                } else if ((li == .Moved or li == .Both) and di != .Moved) {
                    if (m.from == .a1)
                        self.castle.wq = false
                    else if (m.from == .h1)
                        self.castle.wk = false
                    else if (m.from == .a8)
                        self.castle.bq = false
                    else if (m.from == .h8)
                        self.castle.bk = false;
                } else if (self.w_king == m.from) {
                    self.w_king = m.to;
                    self.castle.wk = false;
                    self.castle.wq = false;
                    self.hash[self.hash_in] ^= zbr.ZobristTable[5][@intFromEnum(m.from)];
                    self.hash[self.hash_in] ^= zbr.ZobristTable[5][@intFromEnum(m.to)];
                } else if (self.b_king == m.from) {
                    self.b_king = m.to;
                    self.castle.bk = false;
                    self.castle.bq = false;
                    self.hash[self.hash_in] ^= zbr.ZobristTable[5][@intFromEnum(m.from)];
                    self.hash[self.hash_in] ^= zbr.ZobristTable[5][@intFromEnum(m.to)];
                }
                if ((li == .Removed or li == .Both) and di != .Removed and di != .Both) {
                    if (m.to == .a8)
                        self.castle.bq = false
                    else if (m.to == .h8)
                        self.castle.bk = false
                    else if (m.to == .a1)
                        self.castle.wq = false
                    else if (m.to == .h1)
                        self.castle.wk = false;
                }

                if (white == .Removed or
                    white == .Both or
                    black == .Removed or
                    black == .Both)
                {
                    if (pa == .Removed or pa == .Both)
                        ret = .Pawn
                    else if (di == .Removed or di == .Both)
                        ret = if (li == .Removed or li == .Both) .Queen else .Bishop
                    else if (li == .Removed or li == .Both)
                        ret = .Rook
                    else
                        ret = .Knight;
                }
            },
            .EnPassant => {
                if (self.side == .White) {
                    _ = self.w_pieces.move(m.from, m.to);
                    self.hash[self.hash_in] ^= zbr.ZobristTable[0][@intFromEnum(m.from)];
                    self.hash[self.hash_in] ^= zbr.ZobristTable[0][@intFromEnum(m.to)];

                    const hit = m.to.getApply(.South);
                    _ = self.b_pieces.unset(hit);
                    self.hash[self.hash_in] ^= zbr.ZobristTable[1][@intFromEnum(hit)];

                    _ = self.pawns.unset(hit).move(m.from, m.to);
                    self.hash[self.hash_in] ^= zbr.ZobristTable[2][@intFromEnum(hit)];
                    self.hash[self.hash_in] ^= zbr.ZobristTable[2][@intFromEnum(m.from)];
                } else {
                    _ = self.b_pieces.move(m.from, m.to);
                    self.hash[self.hash_in] ^= zbr.ZobristTable[1][@intFromEnum(m.from)];
                    self.hash[self.hash_in] ^= zbr.ZobristTable[1][@intFromEnum(m.to)];

                    const hit = m.to.getApply(.North);
                    _ = self.w_pieces.unset(hit);
                    self.hash[self.hash_in] ^= zbr.ZobristTable[0][@intFromEnum(hit)];

                    _ = self.pawns.unset(hit).move(m.from, m.to);
                    self.hash[self.hash_in] ^= zbr.ZobristTable[2][@intFromEnum(hit)];
                    self.hash[self.hash_in] ^= zbr.ZobristTable[2][@intFromEnum(m.from)];
                }
            },
            .CastleKingside => {
                _ = self.w_pieces.move(m.from, m.to);
                _ = self.b_pieces.move(m.from, m.to);
                if (self.side == .White) self.w_king = m.to else self.b_king = m.to;

                if (self.side == .White) {
                    self.hash[self.hash_in] ^= zbr.ZobristTable[0][@intFromEnum(m.from)];
                    self.hash[self.hash_in] ^= zbr.ZobristTable[0][@intFromEnum(m.to)];
                } else {
                    self.hash[self.hash_in] ^= zbr.ZobristTable[1][@intFromEnum(m.from)];
                    self.hash[self.hash_in] ^= zbr.ZobristTable[1][@intFromEnum(m.to)];
                }
                self.hash[self.hash_in] ^= zbr.ZobristTable[5][@intFromEnum(m.from)];
                self.hash[self.hash_in] ^= zbr.ZobristTable[5][@intFromEnum(m.to)];

                const fr: tp.Square = if (self.side == .White) .h1 else .h8;
                const to: tp.Square = if (self.side == .White) .f1 else .f8;
                _ = self.w_pieces.move(fr, to);
                _ = self.b_pieces.move(fr, to);
                _ = self.lines.move(fr, to);

                if (self.side == .White) {
                    self.hash[self.hash_in] ^= zbr.ZobristTable[0][@intFromEnum(fr)];
                    self.hash[self.hash_in] ^= zbr.ZobristTable[0][@intFromEnum(to)];
                } else {
                    self.hash[self.hash_in] ^= zbr.ZobristTable[1][@intFromEnum(fr)];
                    self.hash[self.hash_in] ^= zbr.ZobristTable[1][@intFromEnum(to)];
                }
                self.hash[self.hash_in] ^= zbr.ZobristTable[4][@intFromEnum(fr)];
                self.hash[self.hash_in] ^= zbr.ZobristTable[4][@intFromEnum(to)];

                if (self.side == .White) {
                    self.castle.wk = false;
                    self.castle.wq = false;
                } else {
                    self.castle.bk = false;
                    self.castle.bq = false;
                }
            },
            .CastleQueenside => {
                _ = self.w_pieces.move(m.from, m.to);
                _ = self.b_pieces.move(m.from, m.to);
                if (self.side == .White) self.w_king = m.to else self.b_king = m.to;

                if (self.side == .White) {
                    self.hash[self.hash_in] ^= zbr.ZobristTable[0][@intFromEnum(m.from)];
                    self.hash[self.hash_in] ^= zbr.ZobristTable[0][@intFromEnum(m.to)];
                } else {
                    self.hash[self.hash_in] ^= zbr.ZobristTable[1][@intFromEnum(m.from)];
                    self.hash[self.hash_in] ^= zbr.ZobristTable[1][@intFromEnum(m.to)];
                }
                self.hash[self.hash_in] ^= zbr.ZobristTable[5][@intFromEnum(m.from)];
                self.hash[self.hash_in] ^= zbr.ZobristTable[5][@intFromEnum(m.to)];

                const fr: tp.Square = if (self.side == .White) .a1 else .a8;
                const to: tp.Square = if (self.side == .White) .d1 else .d8;
                _ = self.w_pieces.move(fr, to);
                _ = self.b_pieces.move(fr, to);
                _ = self.lines.move(fr, to);

                if (self.side == .White) {
                    self.hash[self.hash_in] ^= zbr.ZobristTable[0][@intFromEnum(fr)];
                    self.hash[self.hash_in] ^= zbr.ZobristTable[0][@intFromEnum(to)];
                } else {
                    self.hash[self.hash_in] ^= zbr.ZobristTable[1][@intFromEnum(fr)];
                    self.hash[self.hash_in] ^= zbr.ZobristTable[1][@intFromEnum(to)];
                }
                self.hash[self.hash_in] ^= zbr.ZobristTable[4][@intFromEnum(fr)];
                self.hash[self.hash_in] ^= zbr.ZobristTable[4][@intFromEnum(to)];

                if (self.side == .White) {
                    self.castle.wk = false;
                    self.castle.wq = false;
                } else {
                    self.castle.bk = false;
                    self.castle.bq = false;
                }
            },
            .PromKnight => {
                const white = self.w_pieces.move(m.from, m.to);
                const black = self.b_pieces.move(m.from, m.to);
                const di = self.diags.checkUnset(m.to);
                const li = self.lines.checkUnset(m.to);
                _ = self.pawns.unset(m.from);

                if (white == .Moved or white == .Both)
                    self.hash[self.hash_in] ^= zbr.ZobristTable[0][@intFromEnum(m.from)];
                if (white == .Moved or white == .Removed)
                    self.hash[self.hash_in] ^= zbr.ZobristTable[0][@intFromEnum(m.to)];
                if (black == .Moved or black == .Both)
                    self.hash[self.hash_in] ^= zbr.ZobristTable[1][@intFromEnum(m.from)];
                if (black == .Moved or black == .Removed)
                    self.hash[self.hash_in] ^= zbr.ZobristTable[1][@intFromEnum(m.to)];

                self.hash[self.hash_in] ^= zbr.ZobristTable[2][@intFromEnum(m.from)];

                if (white == .Removed or black == .Removed) {
                    if (di == .Removed)
                        ret = if (li == .Removed) .Queen else .Bishop
                    else if (li == .Removed)
                        ret = .Rook
                    else
                        ret = .Knight;
                }
            },
            .PromBishop => {
                const white = self.w_pieces.move(m.from, m.to);
                const black = self.b_pieces.move(m.from, m.to);
                const di = self.diags.checkUnset(m.to);
                const li = self.lines.checkUnset(m.to);
                _ = self.pawns.unset(m.from);
                _ = self.diags.set(m.to);

                if (white == .Moved or white == .Both)
                    self.hash[self.hash_in] ^= zbr.ZobristTable[0][@intFromEnum(m.from)];
                if (white == .Moved or white == .Removed)
                    self.hash[self.hash_in] ^= zbr.ZobristTable[0][@intFromEnum(m.to)];
                if (black == .Moved or black == .Both)
                    self.hash[self.hash_in] ^= zbr.ZobristTable[1][@intFromEnum(m.from)];
                if (black == .Moved or black == .Removed)
                    self.hash[self.hash_in] ^= zbr.ZobristTable[1][@intFromEnum(m.to)];

                self.hash[self.hash_in] ^= zbr.ZobristTable[2][@intFromEnum(m.from)];
                self.hash[self.hash_in] ^= zbr.ZobristTable[3][@intFromEnum(m.to)];

                if (white == .Removed or black == .Removed) {
                    if (di == .Removed)
                        ret = if (li == .Removed) .Queen else .Bishop
                    else if (li == .Removed)
                        ret = .Rook
                    else
                        ret = .Knight;
                }
            },
            .PromRook => {
                const white = self.w_pieces.move(m.from, m.to);
                const black = self.b_pieces.move(m.from, m.to);
                const di = self.diags.checkUnset(m.to);
                const li = self.lines.checkUnset(m.to);
                _ = self.pawns.unset(m.from);
                _ = self.lines.set(m.to);

                if (white == .Moved or white == .Both)
                    self.hash[self.hash_in] ^= zbr.ZobristTable[0][@intFromEnum(m.from)];
                if (white == .Moved or white == .Removed)
                    self.hash[self.hash_in] ^= zbr.ZobristTable[0][@intFromEnum(m.to)];
                if (black == .Moved or black == .Both)
                    self.hash[self.hash_in] ^= zbr.ZobristTable[1][@intFromEnum(m.from)];
                if (black == .Moved or black == .Removed)
                    self.hash[self.hash_in] ^= zbr.ZobristTable[1][@intFromEnum(m.to)];

                self.hash[self.hash_in] ^= zbr.ZobristTable[2][@intFromEnum(m.from)];
                self.hash[self.hash_in] ^= zbr.ZobristTable[4][@intFromEnum(m.to)];

                if (white == .Removed or black == .Removed) {
                    if (di == .Removed)
                        ret = if (li == .Removed) .Queen else .Bishop
                    else if (li == .Removed)
                        ret = .Rook
                    else
                        ret = .Knight;
                }
            },
            .PromQueen => {
                const white = self.w_pieces.move(m.from, m.to);
                const black = self.b_pieces.move(m.from, m.to);
                const di = self.diags.checkUnset(m.to);
                const li = self.lines.checkUnset(m.to);
                _ = self.pawns.unset(m.from);
                _ = self.diags.set(m.to);
                _ = self.lines.set(m.to);

                if (white == .Moved or white == .Both)
                    self.hash[self.hash_in] ^= zbr.ZobristTable[0][@intFromEnum(m.from)];
                if (white == .Moved or white == .Removed)
                    self.hash[self.hash_in] ^= zbr.ZobristTable[0][@intFromEnum(m.to)];
                if (black == .Moved or black == .Both)
                    self.hash[self.hash_in] ^= zbr.ZobristTable[1][@intFromEnum(m.from)];
                if (black == .Moved or black == .Removed)
                    self.hash[self.hash_in] ^= zbr.ZobristTable[1][@intFromEnum(m.to)];

                self.hash[self.hash_in] ^= zbr.ZobristTable[2][@intFromEnum(m.from)];
                self.hash[self.hash_in] ^= zbr.ZobristTable[3][@intFromEnum(m.to)];
                self.hash[self.hash_in] ^= zbr.ZobristTable[4][@intFromEnum(m.to)];

                if (white == .Removed or black == .Removed) {
                    if (di == .Removed)
                        ret = if (li == .Removed) .Queen else .Bishop
                    else if (li == .Removed)
                        ret = .Rook
                    else
                        ret = .Knight;
                }
            },
        }

        if (ret != null) self.move_rule = 0;

        if (cas.wk != self.castle.wk) self.hash[self.hash_in] ^= zbr.CastleTable[0];
        if (cas.wq != self.castle.wq) self.hash[self.hash_in] ^= zbr.CastleTable[1];
        if (cas.bk != self.castle.bk) self.hash[self.hash_in] ^= zbr.CastleTable[2];
        if (cas.bq != self.castle.bq) self.hash[self.hash_in] ^= zbr.CastleTable[3];

        self.side = @enumFromInt(~@intFromEnum(self.side));
        return .{ .typ = ret, .pas = pas, .cas = cas, .move_rule = move_rule };
    }

    pub fn remove(self: *Board, m: tp.Move, u: tp.Remove) void {
        self.hash_in -= 1;

        self.side = @enumFromInt(~@intFromEnum(self.side));
        self.pawns.v &= self.w_pieces.v | self.b_pieces.v; //Unsetting old en passant

        switch (m.typ) {
            .Normal => {
                _ = self.w_pieces.move(m.to, m.from);
                _ = self.b_pieces.move(m.to, m.from);
                _ = self.pawns.move(m.to, m.from);
                _ = self.diags.move(m.to, m.from);
                _ = self.lines.move(m.to, m.from);
                if (self.w_king == m.to)
                    self.w_king = m.from
                else if (self.b_king == m.to)
                    self.b_king = m.from;
            },
            .EnPassant => {
                _ = self.w_pieces.move(m.to, m.from);
                _ = self.b_pieces.move(m.to, m.from);

                if (self.side == .White) {
                    const hit = m.to.getApply(.South);
                    _ = self.b_pieces.set(hit);
                    _ = self.pawns.set(hit).move(m.to, m.from);
                } else {
                    const hit = m.to.getApply(.North);
                    _ = self.w_pieces.set(hit);
                    _ = self.pawns.set(hit).move(m.to, m.from);
                }
            },
            .CastleKingside => {
                _ = self.w_pieces.move(m.to, m.from);
                _ = self.b_pieces.move(m.to, m.from);
                if (self.side == .White) self.w_king = m.from else self.b_king = m.from;

                const fr: tp.Square = if (self.side == .White) .h1 else .h8;
                const to: tp.Square = if (self.side == .White) .f1 else .f8;
                if (self.side == .White)
                    _ = self.w_pieces.move(to, fr)
                else
                    _ = self.b_pieces.move(to, fr);
                _ = self.lines.move(to, fr);
            },
            .CastleQueenside => {
                _ = self.w_pieces.move(m.to, m.from);
                _ = self.b_pieces.move(m.to, m.from);
                if (self.side == .White) self.w_king = m.from else self.b_king = m.from;

                const fr: tp.Square = if (self.side == .White) .a1 else .a8;
                const to: tp.Square = if (self.side == .White) .d1 else .d8;
                if (self.side == .White)
                    _ = self.w_pieces.move(to, fr)
                else
                    _ = self.b_pieces.move(to, fr);
                _ = self.lines.move(to, fr);
            },
            .PromKnight => {
                if (self.side == .White)
                    _ = self.w_pieces.move(m.to, m.from)
                else
                    _ = self.b_pieces.move(m.to, m.from);
                _ = self.pawns.set(m.from);
            },
            .PromBishop => {
                if (self.side == .White)
                    _ = self.w_pieces.move(m.to, m.from)
                else
                    _ = self.b_pieces.move(m.to, m.from);
                _ = self.pawns.set(m.from);
                _ = self.diags.unset(m.to);
            },
            .PromRook => {
                if (self.side == .White)
                    _ = self.w_pieces.move(m.to, m.from)
                else
                    _ = self.b_pieces.move(m.to, m.from);
                _ = self.pawns.set(m.from);
                _ = self.lines.unset(m.to);
            },
            .PromQueen => {
                if (self.side == .White)
                    _ = self.w_pieces.move(m.to, m.from)
                else
                    _ = self.b_pieces.move(m.to, m.from);
                _ = self.pawns.set(m.from);
                _ = self.diags.unset(m.to);
                _ = self.lines.unset(m.to);
            },
        }

        if (u.typ) |typ| {
            if (self.side == .White)
                _ = self.b_pieces.set(m.to)
            else
                _ = self.w_pieces.set(m.to);
            switch (typ) {
                .Pawn => _ = self.pawns.set(m.to),
                .Knight => {},
                .Bishop => _ = self.diags.set(m.to),
                .Rook => _ = self.lines.set(m.to),
                .Queen => {
                    _ = self.diags.set(m.to);
                    _ = self.lines.set(m.to);
                },
                else => unreachable,
            }
        }

        self.move_rule = u.move_rule;
        self.castle = u.cas;
        if (u.pas) |pas| {
            self.pawns.v |= pas.toBoard().v; //Resetting en passant
        }
    }

    pub fn print(b: *const Board) void {
        switch (b.side) {
            .White => std.debug.print("w", .{}),
            .Black => std.debug.print("b", .{}),
        }
        std.debug.print(" a b c d e f g h\n", .{});
        for (0..8) |in| {
            for (0..8) |j| {
                const i = 7 - in;

                if (j == 0) std.debug.print("{} ", .{i + 1});
                const ch = b.check(tp.Square.new(@enumFromInt(i), @enumFromInt(j)));
                if (ch) |che| {
                    const pi = if (che.white)
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
