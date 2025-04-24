// Always minimize the number of appends to the arraylist

const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const ta = @import("tablegen.zig");

pub fn printList(list: *std.ArrayList(tp.Move)) void {
    for (list.items) |i| {
        i.print();
        std.debug.print("\n", .{});
    }
}

const PinState = enum { None, Diag, Line };
fn genPawn(
    b: *const bo.Board,
    sq: tp.Square,
    list: *std.ArrayList(tp.Move),
    p: PinState,
    al: tp.BitBoard,
) !void {
    const combi = b.o_pieces.op_or(b.t_pieces);
    const north = sq.getApply(.North);

    if (p != .Diag and (p != .Line or sq.rank() != b.o_king.rank())) {
        // Main squares
        if (!combi.check(north)) {
            const move = tp.Move{ .from = sq, .to = north, .typ = .Normal };
            if (sq.rank() == .Rank2 and !combi.check(sq.getApply(.NorthNorth))) {
                const north2 = sq.getApply(.NorthNorth);
                const move2 = tp.Move{ .from = sq, .to = north2, .typ = .Normal };
                if (al.check(north) and al.check(north2))
                    try list.appendSlice(&[_]tp.Move{ move, move2 })
                else if (al.check(north))
                    try list.append(move)
                else if (al.check(north2))
                    try list.append(move2);
            } else {
                if (al.check(north)) {
                    if (north.rank() == .Rank8) {
                        try list.appendSlice(&[_]tp.Move{
                            .{ .from = sq, .to = north, .typ = .PromKnight },
                            .{ .from = sq, .to = north, .typ = .PromBishop },
                            .{ .from = sq, .to = north, .typ = .PromRook },
                            .{ .from = sq, .to = north, .typ = .PromQueen },
                        });
                    } else try list.append(move);
                }
            }
        }
    }

    if (p != .Line) {
        const diag_pin = if (sq.diagonal() == b.o_king.diagonal()) true else false;
        var hit = ta.PawnAttacks[@intFromEnum(sq)].op_and(b.t_pieces);
        while (hit.popLsb()) |to| {
            if (p == .Diag and diag_pin != (to.diagonal() == sq.diagonal())) continue;

            if (al.check(to)) {
                if (to.rank() == .Rank8) {
                    try list.appendSlice(&[_]tp.Move{
                        .{ .from = sq, .to = to, .typ = .PromKnight },
                        .{ .from = sq, .to = to, .typ = .PromBishop },
                        .{ .from = sq, .to = to, .typ = .PromRook },
                        .{ .from = sq, .to = to, .typ = .PromQueen },
                    });
                } else try list.append(.{ .from = sq, .to = to, .typ = .Normal });
            }
        }

        const en_passant = ta.PawnAttacks[@intFromEnum(sq)]
            .op_and(b.pawns)
            .without(b.o_pieces)
            .without(b.t_pieces);
        if (en_passant.lsb()) |to| {
            if (p != .Diag or diag_pin == (to.diagonal() == sq.diagonal())) {
                if (al.check(to))
                    try list.append(.{ .from = sq, .to = to, .typ = .EnPassant });
            }
        }
    }
}

fn genKing(b: *const bo.Board, sq: tp.Square, list: *std.ArrayList(tp.Move)) !void {
    var iter = ta.KingAttacks[@intFromEnum(sq)].without(b.o_pieces);
    while (iter.popLsb()) |to| {
        if (attack_count(b, to, b.o_king) == 0)
            try list.append(.{ .from = sq, .to = to, .typ = .Normal });
    }
}

fn genKnight(
    b: *const bo.Board,
    sq: tp.Square,
    list: *std.ArrayList(tp.Move),
    p: PinState,
    al: tp.BitBoard,
) !void {
    if (p == .None) {
        var iter = ta.KnightAttacks[@intFromEnum(sq)].without(b.o_pieces);
        while (iter.popLsb()) |to| {
            if (al.check(to))
                try list.append(.{ .from = sq, .to = to, .typ = .Normal });
        }
    }
}

fn genLine(
    b: *const bo.Board,
    sq: tp.Square,
    list: *std.ArrayList(tp.Move),
    p: PinState,
    al: tp.BitBoard,
) !void {
    if (p != .Diag) {
        const combi = b.o_pieces.op_or(b.t_pieces).op_and(ta.getLineMask(sq));

        var iter = ta.getLine(sq, combi).without(b.o_pieces);
        if (p == .Line) {
            const rank_pin = if (sq.rank() == b.o_king.rank()) true else false;
            while (iter.popLsb()) |to| {
                if (rank_pin != (to.rank() == sq.rank())) continue;

                if (al.check(to))
                    try list.append(.{ .from = sq, .to = to, .typ = .Normal });
            }
        } else {
            while (iter.popLsb()) |to| {
                if (al.check(to))
                    try list.append(.{ .from = sq, .to = to, .typ = .Normal });
            }
        }
    }
}

fn genDiag(
    b: *const bo.Board,
    sq: tp.Square,
    list: *std.ArrayList(tp.Move),
    p: PinState,
    al: tp.BitBoard,
) !void {
    if (p != .Line) {
        const combi = b.o_pieces.op_or(b.t_pieces).op_and(ta.getDiagMask(sq));

        var iter = ta.getDiag(sq, combi).without(b.o_pieces);
        if (p == .Diag) {
            const diag_pin = if (sq.diagonal() == b.o_king.diagonal()) true else false;
            while (iter.popLsb()) |to| {
                if (diag_pin != (to.diagonal() == sq.diagonal())) continue;

                if (al.check(to))
                    try list.append(.{ .from = sq, .to = to, .typ = .Normal });
            }
        } else {
            while (iter.popLsb()) |to| {
                if (al.check(to))
                    try list.append(.{ .from = sq, .to = to, .typ = .Normal });
            }
        }
    }
}

const Attacks = struct {
    count: usize,
    hitables: tp.BitBoard,
    lines: tp.BitBoard,
    diags: tp.BitBoard,
};
fn attack_count_complex(b: *const bo.Board, sq: tp.Square) Attacks {
    var ret: usize = 0;
    var hit = tp.BitBoard.new();

    const knights = ta.KnightAttacks[@intFromEnum(sq)].op_and(b.t_pieces
        .without(b.pawns)
        .without(b.lines)
        .without(b.diags)
        .without(b.t_king.toBoard()));
    hit.v |= knights.v;
    ret += knights.popcount();

    const pawns = ta.PawnAttacks[@intFromEnum(sq)]
        .op_and(b.t_pieces.op_and(b.pawns));
    hit.v |= pawns.v;
    ret += pawns.popcount();

    const line = ta.getLine(sq, (b.o_pieces.op_or(b.t_pieces)).op_and(ta.getLineMask(sq)));
    const diag = ta.getDiag(sq, (b.o_pieces.op_or(b.t_pieces)).op_and(ta.getDiagMask(sq)));

    var line_hit = line.op_and(b.t_pieces).op_and(b.lines);
    var diag_hit = diag.op_and(b.t_pieces).op_and(b.diags);

    while (line_hit.popLsb()) |s| {
        ret += 1;
        hit.v |= ta.getLine(s, sq.toBoard().op_and(ta.getLineMask(s)))
            .op_and(line)
            .op_or(s.toBoard()).v;
    }
    while (diag_hit.popLsb()) |s| {
        ret += 1;
        hit.v |= ta.getDiag(s, sq.toBoard().op_and(ta.getDiagMask(s)))
            .op_and(diag)
            .op_or(s.toBoard()).v;
    }

    return .{ .count = ret, .hitables = hit, .lines = line, .diags = diag };
}

pub fn attack_count(b: *const bo.Board, sq: tp.Square, ig: tp.Square) usize {
    var ret: usize = 0;

    if (ta.KingAttacks[@intFromEnum(sq)].check(b.t_king)) ret += 1;

    const knights = ta.KnightAttacks[@intFromEnum(sq)].op_and(b.t_pieces
        .without(b.pawns)
        .without(b.lines)
        .without(b.diags)
        .without(b.t_king.toBoard()));
    ret += knights.popcount();

    const pawns = ta.PawnAttacks[@intFromEnum(sq)]
        .op_and(b.t_pieces.op_and(b.pawns));
    ret += pawns.popcount();

    const line = ta.getLine(sq, (b.o_pieces.op_or(b.t_pieces).without(ig.toBoard()))
        .op_and(ta.getLineMask(sq)));
    const diag = ta.getDiag(sq, (b.o_pieces.op_or(b.t_pieces).without(ig.toBoard()))
        .op_and(ta.getDiagMask(sq)));
    const line_hit = line.op_and(b.t_pieces).op_and(b.lines);
    const diag_hit = diag.op_and(b.t_pieces).op_and(b.diags);
    ret += line_hit.popcount();
    ret += diag_hit.popcount();

    return ret;
}

pub fn gen(b: *const bo.Board, list: *std.ArrayList(tp.Move)) !usize {
    const checks = attack_count_complex(b, b.o_king);

    const pot_pinned_line = checks.lines.op_and(b.o_pieces);
    const pot_pinned_diag = checks.diags.op_and(b.o_pieces);

    var pin_line =
        ta.getLine(b.o_king, (b.o_pieces.op_or(b.t_pieces)).without(pot_pinned_line)
        .op_and(ta.getLineMask(b.o_king))).op_and(b.t_pieces).op_and(b.lines);
    var pin_diag =
        ta.getDiag(b.o_king, (b.o_pieces.op_or(b.t_pieces)).without(pot_pinned_diag)
        .op_and(ta.getDiagMask(b.o_king))).op_and(b.t_pieces).op_and(b.diags);

    var pinned_line = tp.BitBoard.new();
    var pinned_diag = tp.BitBoard.new();
    while (pin_line.popLsb()) |pin| {
        pinned_line.v |= ta.getLine(pin, pot_pinned_line.op_and(ta.getLineMask(pin)))
            .op_and(pot_pinned_line).v;
    }
    while (pin_diag.popLsb()) |pin| {
        pinned_diag.v |= ta.getDiag(pin, pot_pinned_diag.op_and(ta.getDiagMask(pin)))
            .op_and(pot_pinned_diag).v;
    }

    const al = if (checks.count == 1) checks.hitables else tp.BitBoard.newFilled();

    var iter = b.o_pieces;
    while (iter.popLsb()) |sq| {
        const pin_state: PinState =
            if (pinned_line.check(sq))
            .Line
        else if (pinned_diag.check(sq))
            .Diag
        else
            .None;

        if (b.o_king == sq)
            try genKing(b, sq, list)
        else if (checks.count < 2) {
            if (b.pawns.check(sq))
                try genPawn(b, sq, list, pin_state, al)
            else {
                const lin = b.lines.check(sq);
                const dia = b.diags.check(sq);

                if (lin) try genLine(b, sq, list, pin_state, al);
                if (dia) try genDiag(b, sq, list, pin_state, al);
                if (!lin and !dia) try genKnight(b, sq, list, pin_state, al);
            }
        }
    }

    // Castling
    if (checks.count == 0) {
        const combi = b.o_pieces.op_or(b.t_pieces);

        if (b.castle.ok) {
            if (b.mir.horiz) {
                if (!combi.check(.c1) and
                    !combi.check(.b1) and
                    attack_count(b, .c1, .c1) == 0 and
                    attack_count(b, .b1, .b1) == 0)
                    try list.append(.{ .from = .d1, .to = .b1, .typ = .CastleKingside });
            } else {
                if (!combi.check(.f1) and
                    !combi.check(.g1) and
                    attack_count(b, .f1, .f1) == 0 and
                    attack_count(b, .g1, .g1) == 0)
                    try list.append(.{ .from = .e1, .to = .g1, .typ = .CastleKingside });
            }
        }
        if (b.castle.oq) {
            if (b.mir.horiz) {
                if (!combi.check(.e1) and
                    !combi.check(.f1) and
                    !combi.check(.g1) and
                    attack_count(b, .e1, .e1) == 0 and
                    attack_count(b, .f1, .f1) == 0)
                    try list.append(.{ .from = .d1, .to = .f1, .typ = .CastleQueenside });
            } else {
                if (!combi.check(.d1) and
                    !combi.check(.c1) and
                    !combi.check(.b1) and
                    attack_count(b, .d1, .d1) == 0 and
                    attack_count(b, .c1, .c1) == 0)
                    try list.append(.{ .from = .e1, .to = .c1, .typ = .CastleQueenside });
            }
        }
    }

    return checks.count;
}

pub fn gen_with_checks(b: *const bo.Board, list: *std.ArrayList(tp.Move)) !usize {
    const checks = attack_count_complex(b, b.o_king);
    if (checks.count == 0) return 0;

    const pot_pinned_line = checks.lines.op_and(b.o_pieces);
    const pot_pinned_diag = checks.diags.op_and(b.o_pieces);

    var pin_line =
        ta.getLine(b.o_king, (b.o_pieces.op_or(b.t_pieces)).without(pot_pinned_line)
        .op_and(ta.getLineMask(b.o_king))).op_and(b.t_pieces).op_and(b.lines);
    var pin_diag =
        ta.getDiag(b.o_king, (b.o_pieces.op_or(b.t_pieces)).without(pot_pinned_diag)
        .op_and(ta.getDiagMask(b.o_king))).op_and(b.t_pieces).op_and(b.diags);

    var pinned_line = tp.BitBoard.new();
    var pinned_diag = tp.BitBoard.new();
    while (pin_line.popLsb()) |pin| {
        pinned_line.v |= ta.getLine(pin, pot_pinned_line.op_and(ta.getLineMask(pin)))
            .op_and(pot_pinned_line).v;
    }
    while (pin_diag.popLsb()) |pin| {
        pinned_diag.v |= ta.getDiag(pin, pot_pinned_diag.op_and(ta.getDiagMask(pin)))
            .op_and(pot_pinned_diag).v;
    }

    const al = if (checks.count == 1) checks.hitables else tp.BitBoard.newFilled();

    var iter = b.o_pieces;
    while (iter.popLsb()) |sq| {
        const pin_state: PinState =
            if (pinned_line.check(sq))
            .Line
        else if (pinned_diag.check(sq))
            .Diag
        else
            .None;

        if (b.o_king == sq)
            try genKing(b, sq, list)
        else if (checks.count < 2) {
            if (b.pawns.check(sq))
                try genPawn(b, sq, list, pin_state, al)
            else {
                const lin = b.lines.check(sq);
                const dia = b.diags.check(sq);

                if (lin) try genLine(b, sq, list, pin_state, al);
                if (dia) try genDiag(b, sq, list, pin_state, al);
                if (!lin and !dia) try genKnight(b, sq, list, pin_state, al);
            }
        }
    }

    // Castling
    if (checks.count == 0) {
        const combi = b.o_pieces.op_or(b.t_pieces);

        if (b.castle.ok) {
            if (b.mir.horiz) {
                if (!combi.check(.c1) and
                    !combi.check(.b1) and
                    attack_count(b, .c1, .c1) == 0 and
                    attack_count(b, .b1, .b1) == 0)
                    try list.append(.{ .from = .d1, .to = .b1, .typ = .CastleKingside });
            } else {
                if (!combi.check(.f1) and
                    !combi.check(.g1) and
                    attack_count(b, .f1, .f1) == 0 and
                    attack_count(b, .g1, .g1) == 0)
                    try list.append(.{ .from = .e1, .to = .g1, .typ = .CastleKingside });
            }
        }
        if (b.castle.oq) {
            if (b.mir.horiz) {
                if (!combi.check(.e1) and
                    !combi.check(.f1) and
                    !combi.check(.g1) and
                    attack_count(b, .e1, .e1) == 0 and
                    attack_count(b, .f1, .f1) == 0)
                    try list.append(.{ .from = .d1, .to = .f1, .typ = .CastleQueenside });
            } else {
                if (!combi.check(.d1) and
                    !combi.check(.c1) and
                    !combi.check(.b1) and
                    attack_count(b, .d1, .d1) == 0 and
                    attack_count(b, .c1, .c1) == 0)
                    try list.append(.{ .from = .e1, .to = .c1, .typ = .CastleQueenside });
            }
        }
    }

    return checks.count;
}
