// Always minimize the number of appends to the arraylist

const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const ta = @import("tablegen.zig");

pub fn printList(list: *std.ArrayList(tp.Move)) void {
    var counter: u8 = 1;
    for (list.items) |i| {
        std.debug.print("{}. ", .{counter});
        i.print();
        std.debug.print("\n", .{});
        counter += 1;
    }
}

const PinState = enum { None, Diag, Line };
pub const Data = packed struct {
    combi: tp.BitBoard,
    our: tp.BitBoard,
    their: tp.BitBoard,
    our_king: tp.Square,
};

pub const MoveType = enum { Quiet, Capture, Either, Castle };

pub const Maker = struct {
    b: *const bo.Board,
    dat: Data,
    al: tp.BitBoard,
    checks: usize,
    pinned_line: tp.BitBoard,
    pinned_diag: tp.BitBoard,

    inline fn genPawn(
        self: *const Maker,
        sq: tp.Square,
        list: *std.ArrayList(tp.Move),
        p: PinState,
        cap: MoveType,
    ) !void {
        const next = if (self.b.side == .White) sq.getApply(.North) else sq.getApply(.South);

        const prom_rank = if (self.b.side == .White) tp.Rank.Rank8 else tp.Rank.Rank1;
        const double_rank = if (self.b.side == .White) tp.Rank.Rank2 else tp.Rank.Rank7;

        if (cap != .Capture and
            p != .Diag and
            (p != .Line or sq.rank() != self.dat.our_king.rank()))
        {
            // Main squares
            if (!self.dat.combi.check(next)) {
                const move = tp.Move{ .from = sq, .to = next, .typ = .Normal };
                const next2 = if (self.b.side == .White)
                    sq.getApplySafe(.NorthNorth)
                else
                    sq.getApplySafe(.SouthSouth);
                if (sq.rank() == double_rank and !self.dat.combi.check(next2.?)) {
                    const move2 = tp.Move{ .from = sq, .to = next2.?, .typ = .Normal };

                    if (self.al.check(next) and self.al.check(next2.?))
                        try list.appendSlice(&[_]tp.Move{ move, move2 })
                    else if (self.al.check(next))
                        try list.append(move)
                    else if (self.al.check(next2.?))
                        try list.append(move2);
                } else if (self.al.check(next)) {
                    if (next.rank() == prom_rank) {
                        try list.appendSlice(&[_]tp.Move{
                            .{ .from = sq, .to = next, .typ = .PromKnight },
                            .{ .from = sq, .to = next, .typ = .PromBishop },
                            .{ .from = sq, .to = next, .typ = .PromRook },
                            .{ .from = sq, .to = next, .typ = .PromQueen },
                        });
                    } else try list.append(move);
                }
            }
        }

        if (cap != .Quiet and p != .Line) {
            var hit = (if (self.b.side == .White)
                ta.PawnAttacksWhite[@intFromEnum(sq)].op_and(self.dat.their)
            else
                ta.PawnAttacksBlack[@intFromEnum(sq)].op_and(self.dat.their))
                .op_and(self.al);
            if (p == .Diag) {
                if (sq.diagonal() == self.dat.our_king.diagonal())
                    hit.v &= tp.DiagonalMask[sq.diagonal()].v
                else
                    hit.v &= tp.AntiDiagonalMask[sq.antiDiagonal()].v;
            }
            const num = if (next.rank() == prom_rank) hit.popcount() else hit.popcount() * 4;
            try list.ensureUnusedCapacity(num);
            while (hit.popLsb()) |to| {
                if (to.rank() == prom_rank) {
                    list.appendSliceAssumeCapacity(&[_]tp.Move{
                        .{ .from = sq, .to = to, .typ = .PromKnight },
                        .{ .from = sq, .to = to, .typ = .PromBishop },
                        .{ .from = sq, .to = to, .typ = .PromRook },
                        .{ .from = sq, .to = to, .typ = .PromQueen },
                    });
                } else list.appendAssumeCapacity(.{ .from = sq, .to = to, .typ = .Normal });
            }

            var en_passant = (if (self.b.side == .White)
                ta.PawnAttacksWhite[@intFromEnum(sq)]
            else
                ta.PawnAttacksBlack[@intFromEnum(sq)])
                .op_and(self.b.enPassant())
                .op_and(self.al);
            if (p == .Diag) {
                if (sq.diagonal() == self.dat.our_king.diagonal())
                    en_passant.v &= tp.DiagonalMask[sq.diagonal()].v
                else
                    en_passant.v &= tp.AntiDiagonalMask[sq.antiDiagonal()].v;
            }
            if (en_passant.lsb()) |to|
                try list.append(.{ .from = sq, .to = to, .typ = .EnPassant });
        }
    }

    inline fn checkPawn(
        self: *const Maker,
        sq: tp.Square,
        p: PinState,
        mv: tp.Move,
    ) bool {
        if (mv.from != sq) return false;

        const next = if (self.b.side == .White) sq.getApply(.North) else sq.getApply(.South);

        const prom_rank = if (self.b.side == .White) tp.Rank.Rank8 else tp.Rank.Rank1;
        const double_rank = if (self.b.side == .White) tp.Rank.Rank2 else tp.Rank.Rank7;

        if (mv.typ.promotion() and mv.to.rank() != prom_rank) return false;

        if (mv.typ != .EnPassant and
            p != .Diag and (p != .Line or sq.rank() != self.dat.our_king.rank()))
        {
            // Main squares
            if (!self.dat.combi.check(next)) {
                const next2 = if (self.b.side == .White)
                    sq.getApplySafe(.NorthNorth)
                else
                    sq.getApplySafe(.SouthSouth);
                if (sq.rank() == double_rank and
                    next2 != null and
                    !self.dat.combi.check(next2.?))
                {
                    if (self.al.check(next) and mv.to == next) return true;
                    if (self.al.check(next2.?) and mv.to == next2) return true;
                } else {
                    if (self.al.check(next)) {
                        if (next.rank() == prom_rank) {
                            if (mv.typ == .PromKnight or mv.typ == .PromBishop or
                                mv.typ == .PromRook or mv.typ == .PromQueen)
                            {
                                if (mv.to == next) return true;
                            }
                        } else if (mv.to == next and mv.typ == .Normal) return true;
                    }
                }
            }
        }

        if (p != .Line) {
            const diag_pin = sq.diagonal() == self.dat.our_king.diagonal();

            if (mv.typ == .EnPassant) {
                const en_passant = (if (self.b.side == .White)
                    ta.PawnAttacksWhite[@intFromEnum(sq)]
                else
                    ta.PawnAttacksBlack[@intFromEnum(sq)])
                    .op_and(self.b.enPassant())
                    .op_and(self.al)
                    .op_and(mv.to.toBoard());
                if (en_passant.lsb()) |to| {
                    if (p != .Diag or diag_pin == (to.diagonal() == sq.diagonal()))
                        return true;
                }
            } else {
                const hit = (if (self.b.side == .White)
                    ta.PawnAttacksWhite[@intFromEnum(sq)]
                else
                    ta.PawnAttacksBlack[@intFromEnum(sq)])
                    .op_and(self.dat.their)
                    .op_and(self.al)
                    .op_and(mv.to.toBoard());
                if (hit.lsb()) |to| {
                    if (p != .Diag or diag_pin == (to.diagonal() == sq.diagonal())) {
                        if (to.rank() == prom_rank) {
                            if (mv.typ == .PromKnight or mv.typ == .PromBishop or
                                mv.typ == .PromRook or mv.typ == .PromQueen)
                            {
                                if (mv.to == to) return true;
                            }
                        } else if (mv.to == to)
                            return true;
                    }
                }
            }
        }

        return false;
    }

    inline fn genKing(
        self: *const Maker,
        sq: tp.Square,
        list: *std.ArrayList(tp.Move),
        cap: MoveType,
    ) !void {
        var iter = ta.KingAttacks[@intFromEnum(sq)]
            .without(self.dat.our);
        if (cap == .Capture)
            iter.v &= self.dat.their.v
        else if (cap == .Quiet)
            iter = iter.without(self.dat.their);
        while (iter.popLsb()) |to| {
            if (self.attack_count(to, self.dat.our_king) == 0)
                try list.append(.{ .from = sq, .to = to, .typ = .Normal });
        }
    }

    inline fn checkKing(
        self: *const Maker,
        sq: tp.Square,
        mv: tp.Move,
    ) bool {
        if (mv.from != sq) return false;
        const iter = ta.KingAttacks[@intFromEnum(sq)]
            .without(self.dat.our)
            .op_and(mv.to.toBoard());

        if (iter.lsb()) |to| return self.attack_count(to, self.dat.our_king) == 0;
        return false;
    }

    inline fn genKnight(
        self: *const Maker,
        sq: tp.Square,
        list: *std.ArrayList(tp.Move),
        p: PinState,
        cap: MoveType,
    ) !void {
        if (p == .None) {
            var iter = ta.KnightAttacks[@intFromEnum(sq)]
                .without(self.dat.our)
                .op_and(self.al);
            if (cap == .Capture)
                iter.v &= self.dat.their.v
            else if (cap == .Quiet)
                iter = iter.without(self.dat.their);
            try list.ensureUnusedCapacity(iter.popcount());
            while (iter.popLsb()) |to|
                list.appendAssumeCapacity(.{ .from = sq, .to = to, .typ = .Normal });
        }
    }

    inline fn checkKnight(
        self: *const Maker,
        sq: tp.Square,
        p: PinState,
        mv: tp.Move,
    ) bool {
        if (mv.from != sq) return false;

        const iter = ta.KnightAttacks[@intFromEnum(sq)]
            .without(self.dat.our)
            .op_and(mv.to.toBoard());
        return p == .None and iter.op_and(self.al).v != 0;
    }

    inline fn genLine(
        self: *const Maker,
        sq: tp.Square,
        list: *std.ArrayList(tp.Move),
        p: PinState,
        cap: MoveType,
    ) !void {
        if (p != .Diag) {
            var iter = ta.getLine(sq, self.dat.combi)
                .without(self.dat.our)
                .op_and(self.al);
            if (cap == .Capture)
                iter.v &= self.dat.their.v
            else if (cap == .Quiet)
                iter = iter.without(self.dat.their);
            if (p == .Line) {
                if (sq.rank() == self.dat.our_king.rank())
                    iter.v &= tp.RankMask[@intFromEnum(sq.rank())].v
                else
                    iter.v &= tp.FileMask[@intFromEnum(sq.file())].v;
            }
            try list.ensureUnusedCapacity(iter.popcount());
            while (iter.popLsb()) |to|
                list.appendAssumeCapacity(.{ .from = sq, .to = to, .typ = .Normal });
        }
    }

    inline fn checkLine(
        self: *const Maker,
        sq: tp.Square,
        p: PinState,
        mv: tp.Move,
    ) bool {
        if (p != .Diag) {
            var iter = ta.getLine(sq, self.dat.combi)
                .without(self.dat.our)
                .op_and(mv.to.toBoard())
                .op_and(self.al);
            if (p == .Line) {
                if (sq.rank() == self.dat.our_king.rank())
                    iter.v &= tp.RankMask[@intFromEnum(sq.rank())].v
                else
                    iter.v &= tp.FileMask[@intFromEnum(sq.file())].v;
            }
            return iter.lsb() != null;
        }

        return false;
    }

    inline fn genDiag(
        self: *const Maker,
        sq: tp.Square,
        list: *std.ArrayList(tp.Move),
        p: PinState,
        cap: MoveType,
    ) !void {
        if (p != .Line) {
            var iter = ta.getDiag(sq, self.dat.combi)
                .without(self.dat.our)
                .op_and(self.al);
            if (cap == .Capture)
                iter.v &= self.dat.their.v
            else if (cap == .Quiet)
                iter = iter.without(self.dat.their);
            if (p == .Diag) {
                if (sq.diagonal() == self.dat.our_king.diagonal())
                    iter.v &= tp.DiagonalMask[sq.diagonal()].v
                else
                    iter.v &= tp.AntiDiagonalMask[sq.antiDiagonal()].v;
            }
            try list.ensureUnusedCapacity(iter.popcount());
            while (iter.popLsb()) |to|
                list.appendAssumeCapacity(.{ .from = sq, .to = to, .typ = .Normal });
        }
    }

    inline fn checkDiag(
        self: *const Maker,
        sq: tp.Square,
        p: PinState,
        mv: tp.Move,
    ) bool {
        if (p != .Line) {
            var iter = ta.getDiag(sq, self.dat.combi)
                .without(self.dat.our)
                .op_and(mv.to.toBoard())
                .op_and(self.al);
            if (p == .Diag) {
                if (sq.diagonal() == self.dat.our_king.diagonal())
                    iter.v &= tp.DiagonalMask[sq.diagonal()].v
                else
                    iter.v &= tp.AntiDiagonalMask[sq.antiDiagonal()].v;
            }
            return iter.lsb() != null;
        }

        return false;
    }

    pub const Attacks = struct {
        count: usize,
        hitables: tp.BitBoard,
        lines: tp.BitBoard,
        diags: tp.BitBoard,
    };
    inline fn attack_count_complex(self: *const Maker, sq: tp.Square) Attacks {
        var ret: usize = 0;
        var hit = tp.BitBoard.new();

        const knights = ta.KnightAttacks[@intFromEnum(sq)].op_and(self.dat.their
            .without(self.b.pawns)
            .without(self.b.lines)
            .without(self.b.diags)
            .without((if (self.b.side == .White) self.b.b_king else self.b.w_king).toBoard()));
        hit.v |= knights.v;
        ret += knights.popcount();

        const pawns = (if (self.b.side == .White)
            ta.PawnAttacksWhite
        else
            ta.PawnAttacksBlack)[@intFromEnum(sq)]
            .op_and(self.dat.their.op_and(self.b.pawns));
        hit.v |= pawns.v;
        ret += pawns.popcount();

        const line = ta.getLine(sq, self.dat.combi);
        const diag = ta.getDiag(sq, self.dat.combi);

        var line_hit = line.op_and(self.dat.their).op_and(self.b.lines);
        var diag_hit = diag.op_and(self.dat.their).op_and(self.b.diags);

        while (line_hit.popLsb()) |s| {
            ret += 1;
            hit.v |= ta.getLine(s, sq.toBoard())
                .op_and(line)
                .op_or(s.toBoard()).v;
        }
        while (diag_hit.popLsb()) |s| {
            ret += 1;
            hit.v |= ta.getDiag(s, sq.toBoard())
                .op_and(diag)
                .op_or(s.toBoard()).v;
        }

        return .{ .count = ret, .hitables = hit, .lines = line, .diags = diag };
    }

    pub inline fn attackers(self: *const Maker, sq: tp.Square) tp.BitBoard {
        var ret = tp.BitBoard.new();

        const kings = self.b.w_king.toBoard().op_or(self.b.b_king.toBoard());
        ret = ret
            .op_or(ta.KingAttacks[@intFromEnum(sq)]
            .op_and(kings));

        const knights = ta.KnightAttacks[@intFromEnum(sq)]
            .without(self.b.pawns)
            .without(self.b.lines)
            .without(self.b.diags)
            .without(kings);
        ret = ret.op_or(knights);

        const pawns = ta.PawnAttacksWhite[@intFromEnum(sq)]
            .op_or(ta.PawnAttacksBlack[@intFromEnum(sq)])
            .op_and(self.b.pawns);
        ret = ret.op_or(pawns);

        const line = ta.getLine(sq, self.dat.combi);
        const diag = ta.getDiag(sq, self.dat.combi);
        const line_hit = line.op_and(self.b.lines);
        const diag_hit = diag.op_and(self.b.diags);
        ret = ret.op_or(line_hit);
        ret = ret.op_or(diag_hit);

        return ret;
    }

    pub inline fn attackersDiag(
        self: *const Maker,
        sq: tp.Square,
        combi: tp.BitBoard,
    ) tp.BitBoard {
        const diag = ta.getDiag(sq, combi);
        const diag_hit = diag.op_and(self.b.diags).op_and(combi);

        return diag_hit;
    }

    pub inline fn attackersLine(
        self: *const Maker,
        sq: tp.Square,
        combi: tp.BitBoard,
    ) tp.BitBoard {
        const line = ta.getLine(sq, combi);
        const line_hit = line.op_and(self.b.lines).op_and(combi);

        return line_hit;
    }

    pub inline fn attackedPawn(self: *const Maker) tp.BitBoard {
        var ret = tp.BitBoard.new();

        var iter = self.dat.their.op_and(self.b.pawns).without(self.pinned_line);
        while (iter.popLsb()) |sq| {
            var hit = (if (self.b.side == .White)
                ta.PawnAttacksBlack[@intFromEnum(sq)]
            else
                ta.PawnAttacksWhite[@intFromEnum(sq)]);
            if (self.pinned_diag.check(sq)) {
                if (sq.diagonal() == self.dat.our_king.diagonal())
                    hit.v &= tp.DiagonalMask[sq.diagonal()].v
                else
                    hit.v &= tp.AntiDiagonalMask[sq.antiDiagonal()].v;
            }

            ret.v |= hit.v;
        }

        return ret;
    }

    fn attack_count(self: *const Maker, sq: tp.Square, ig: tp.Square) usize {
        var ret: usize = 0;

        const t_king = if (self.b.side == .White) self.b.b_king else self.b.w_king;
        if (ta.KingAttacks[@intFromEnum(sq)].check(t_king))
            ret += 1;

        const knights = ta.KnightAttacks[@intFromEnum(sq)].op_and(self.dat.their
            .without(self.b.pawns)
            .without(self.b.lines)
            .without(self.b.diags)
            .without(t_king.toBoard()));
        ret += knights.popcount();

        const pawns = (if (self.b.side == .White)
            ta.PawnAttacksWhite
        else
            ta.PawnAttacksBlack)[@intFromEnum(sq)]
            .op_and(self.dat.their.op_and(self.b.pawns));
        ret += pawns.popcount();

        const line = ta.getLine(sq, self.dat.combi.without(ig.toBoard()));
        const diag = ta.getDiag(sq, self.dat.combi.without(ig.toBoard()));
        const line_hit = line.op_and(self.dat.their).op_and(self.b.lines);
        const diag_hit = diag.op_and(self.dat.their).op_and(self.b.diags);
        ret += line_hit.popcount();
        ret += diag_hit.popcount();

        return ret;
    }

    pub fn init(b: *const bo.Board) Maker {
        var ret = Maker{
            .b = b,
            .dat = .{
                .combi = b.w_pieces.op_or(b.b_pieces),
                .our = if (b.side == .White) b.w_pieces else b.b_pieces,
                .their = if (b.side == .White) b.b_pieces else b.w_pieces,
                .our_king = if (b.side == .White) b.w_king else b.b_king,
            },
            .al = tp.BitBoard.new(),
            .checks = 0,
            .pinned_line = tp.BitBoard.new(),
            .pinned_diag = tp.BitBoard.new(),
        };

        const checks = ret.attack_count_complex(ret.dat.our_king);
        ret.checks = checks.count;

        const pot_pinned_line = checks.lines.op_and(ret.dat.our);
        const pot_pinned_diag = checks.diags.op_and(ret.dat.our);

        var pin_line =
            ta.getLine(ret.dat.our_king, ret.dat.combi.without(pot_pinned_line))
                .op_and(ret.dat.their).op_and(b.lines);
        var pin_diag =
            ta.getDiag(ret.dat.our_king, ret.dat.combi.without(pot_pinned_diag))
                .op_and(ret.dat.their).op_and(b.diags);

        while (pin_line.popLsb()) |pin| {
            ret.pinned_line.v |= ta.getLine(pin, pot_pinned_line)
                .op_and(pot_pinned_line).v;
        }
        while (pin_diag.popLsb()) |pin| {
            ret.pinned_diag.v |= ta.getDiag(pin, pot_pinned_diag)
                .op_and(pot_pinned_diag).v;
        }

        ret.al = if (ret.checks == 1) checks.hitables else tp.BitBoard.newFilled();
        return ret;
    }

    pub inline fn isLegal(self: *const Maker, mv: tp.Move) bool {
        if (self.b.side == .White and
            (!self.b.w_pieces.check(mv.from) or self.b.w_pieces.check(mv.to)))
            return false;
        if (self.b.side == .Black and
            (!self.b.b_pieces.check(mv.from) or self.b.b_pieces.check(mv.to)))
            return false;

        switch (mv.typ) {
            .Normal, .EnPassant, .PromKnight, .PromBishop, .PromRook, .PromQueen => {
                const sq = mv.from;

                const pin_state: PinState =
                    if (self.pinned_line.check(sq))
                        .Line
                    else if (self.pinned_diag.check(sq))
                        .Diag
                    else
                        .None;

                if (self.dat.our_king == sq)
                    return mv.typ == .Normal and self.checkKing(sq, mv)
                else if (self.checks < 2) {
                    if (self.b.pawns.check(sq))
                        return self.checkPawn(sq, pin_state, mv)
                    else if (mv.typ == .Normal) {
                        const lin = self.b.lines.check(sq);
                        const dia = self.b.diags.check(sq);

                        if (lin and dia) return self.checkLine(sq, pin_state, mv) or
                            self.checkDiag(sq, pin_state, mv);
                        if (lin) return self.checkLine(sq, pin_state, mv);
                        if (dia) return self.checkDiag(sq, pin_state, mv);
                        return self.checkKnight(sq, pin_state, mv);
                    }
                }
            },
            .CastleKingside => {
                if (if (self.b.side == .White) self.b.castle.wk else self.b.castle.bk) {
                    if (self.b.side == .White) {
                        return !self.dat.combi.check(.f1) and
                            !self.dat.combi.check(.g1) and
                            self.attack_count(.f1, .f1) == 0 and
                            self.attack_count(.g1, .g1) == 0;
                    } else {
                        return !self.dat.combi.check(.f8) and
                            !self.dat.combi.check(.g8) and
                            self.attack_count(.f8, .f8) == 0 and
                            self.attack_count(.g8, .g8) == 0;
                    }
                }
            },
            .CastleQueenside => {
                if (if (self.b.side == .White) self.b.castle.wq else self.b.castle.bq) {
                    if (self.b.side == .White) {
                        return !self.dat.combi.check(.d1) and
                            !self.dat.combi.check(.c1) and
                            !self.dat.combi.check(.b1) and
                            self.attack_count(.d1, .d1) == 0 and
                            self.attack_count(.c1, .c1) == 0;
                    } else {
                        return !self.dat.combi.check(.d8) and
                            !self.dat.combi.check(.c8) and
                            !self.dat.combi.check(.b8) and
                            self.attack_count(.d8, .d8) == 0 and
                            self.attack_count(.c8, .c8) == 0;
                    }
                }
            },
        }

        return false;
    }

    pub fn gen(self: *const Maker, list: *std.ArrayList(tp.Move), typ: MoveType) !void {
        if (typ != .Castle) {
            var iter = self.dat.our;
            while (iter.popLsb()) |sq| {
                const pin_state: PinState =
                    if (self.pinned_line.check(sq))
                        .Line
                    else if (self.pinned_diag.check(sq))
                        .Diag
                    else
                        .None;

                if (self.dat.our_king == sq)
                    try self.genKing(sq, list, typ)
                else if (self.checks < 2) {
                    if (self.b.pawns.check(sq))
                        try self.genPawn(sq, list, pin_state, typ)
                    else {
                        const lin = self.b.lines.check(sq);
                        const dia = self.b.diags.check(sq);

                        if (lin) try self.genLine(sq, list, pin_state, typ);
                        if (dia) try self.genDiag(sq, list, pin_state, typ);
                        if (!lin and !dia) try self.genKnight(sq, list, pin_state, typ);
                    }
                }
            }
        } else if (self.checks == 0) {
            if (if (self.b.side == .White) self.b.castle.wk else self.b.castle.bk) {
                if (self.b.side == .White) {
                    if (!self.dat.combi.check(.f1) and
                        !self.dat.combi.check(.g1) and
                        self.attack_count(.f1, .f1) == 0 and
                        self.attack_count(.g1, .g1) == 0)
                        try list.append(.{ .from = .e1, .to = .g1, .typ = .CastleKingside });
                } else {
                    if (!self.dat.combi.check(.f8) and
                        !self.dat.combi.check(.g8) and
                        self.attack_count(.f8, .f8) == 0 and
                        self.attack_count(.g8, .g8) == 0)
                        try list.append(.{ .from = .e8, .to = .g8, .typ = .CastleKingside });
                }
            }
            if (if (self.b.side == .White) self.b.castle.wq else self.b.castle.bq) {
                if (self.b.side == .White) {
                    if (!self.dat.combi.check(.d1) and
                        !self.dat.combi.check(.c1) and
                        !self.dat.combi.check(.b1) and
                        self.attack_count(.d1, .d1) == 0 and
                        self.attack_count(.c1, .c1) == 0)
                        try list.append(.{ .from = .e1, .to = .c1, .typ = .CastleQueenside });
                } else {
                    if (!self.dat.combi.check(.d8) and
                        !self.dat.combi.check(.c8) and
                        !self.dat.combi.check(.b8) and
                        self.attack_count(.d8, .d8) == 0 and
                        self.attack_count(.c8, .c8) == 0)
                        try list.append(.{ .from = .e8, .to = .c8, .typ = .CastleQueenside });
                }
            }
        }
    }
};
