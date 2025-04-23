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
    const combi = b.o_pieces.o(b.t_pieces);
    const north = sq.getApply(.North);

    if (p != .Diag) {
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
                if (al.check(north)) try list.append(move);
            }
        }
    }

    if (p != .Line) {
        var hit = ta.PawnAttacks[@intFromEnum(sq)].a(b.t_pieces);
        while (hit.popLsb()) |to| {
            if (al.check(to))
                try list.append(.{ .from = sq, .to = to, .typ = .Normal });
        }

        const en_passant = ta.PawnAttacks[@intFromEnum(sq)]
            .a(b.pawns)
            .without(b.o_pieces)
            .without(b.t_pieces);
        if (en_passant.lsb()) |to| {
            if (al.check(to))
                try list.append(.{ .from = sq, .to = to, .typ = .EnPassant });
        }
    }
}

fn genKing(b: *const bo.Board, sq: tp.Square, list: *std.ArrayList(tp.Move)) !void {
    var iter = ta.KingAttacks[@intFromEnum(sq)].without(b.o_pieces);
    while (iter.popLsb()) |to| {
        if (attack_count(b, to) == 0)
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
        const combi = b.o_pieces.o(b.t_pieces).a(ta.getLineMask(sq));

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
        const combi = b.o_pieces.o(b.t_pieces).a(ta.getDiagMask(sq));

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

    const knights = ta.KnightAttacks[@intFromEnum(sq)].a(b.t_pieces
        .without(b.pawns)
        .without(b.lines)
        .without(b.diags)
        .without(b.t_king.toBoard()));
    hit.v |= knights.v;
    ret += knights.popcount();

    const pawns = ta.PawnAttacks[@intFromEnum(sq)]
        .a(b.t_pieces.a(b.pawns));
    hit.v |= pawns.v;
    ret += pawns.popcount();

    const line = ta.getLine(sq, (b.o_pieces.o(b.t_pieces)).a(ta.getLineMask(sq)));
    const diag = ta.getDiag(sq, (b.o_pieces.o(b.t_pieces)).a(ta.getDiagMask(sq)));

    var line_hit = line.a(b.t_pieces).a(b.lines);
    var diag_hit = diag.a(b.t_pieces).a(b.diags);
    const line_bef = ta.getLine(sq, line_hit).without(line_hit);
    const diag_bef = ta.getDiag(sq, diag_hit).without(diag_hit);

    while (line_hit.popLsb()) |s| {
        ret += 1;
        hit.v |= ta.getLine(s, sq.toBoard()).a(line_bef).v;
    }
    while (diag_hit.popLsb()) |s| {
        ret += 1;
        hit.v |= ta.getDiag(s, sq.toBoard()).a(diag_bef).v;
    }

    return .{ .count = ret, .hitables = hit, .lines = line, .diags = diag };
}

fn attack_count(b: *const bo.Board, sq: tp.Square) usize {
    var ret: usize = 0;

    const knights = ta.KnightAttacks[@intFromEnum(sq)].a(b.t_pieces
        .without(b.pawns)
        .without(b.lines)
        .without(b.diags)
        .without(b.t_king.toBoard()));
    ret += knights.popcount();

    const pawns = ta.PawnAttacks[@intFromEnum(sq)]
        .a(b.t_pieces.a(b.pawns));
    ret += pawns.popcount();

    const line = ta.getLine(sq, (b.o_pieces.o(b.t_pieces)).a(ta.getLineMask(sq)));
    const diag = ta.getDiag(sq, (b.o_pieces.o(b.t_pieces)).a(ta.getDiagMask(sq)));
    const line_hit = line.a(b.t_pieces).a(b.lines);
    const diag_hit = diag.a(b.t_pieces).a(b.diags);
    ret += line_hit.popcount();
    ret += diag_hit.popcount();

    return ret;
}

pub fn gen(b: *const bo.Board, list: *std.ArrayList(tp.Move)) !void {
    const checks = attack_count_complex(b, b.o_king);

    const pinned_line = checks.lines.a(b.o_pieces);
    const pinned_diag = checks.diags.a(b.o_pieces);

    const pin_line =
        ta.getLine(b.o_king, (b.o_pieces.o(b.t_pieces)).without(pinned_line)
        .a(ta.getLineMask(b.o_king))).a(b.t_pieces).a(b.lines);
    const pin_diag =
        ta.getDiag(b.o_king, (b.o_pieces.o(b.t_pieces)).without(pinned_diag)
        .a(ta.getDiagMask(b.o_king))).a(b.t_pieces).a(b.diags);

    const al = if (checks.count == 1) checks.hitables else tp.BitBoard.newFilled();

    var iter = b.o_pieces;
    while (iter.popLsb()) |sq| {
        const pin_state: PinState =
            if (pin_line.check(sq))
            .Line
        else if (pin_diag.check(sq))
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
        if (b.castle.ok) {
            if (b.mir.horiz) {
                if (!b.o_pieces.check(.c1) and
                    !b.o_pieces.check(.b1) and
                    attack_count(b, .c1) == 0 and
                    attack_count(b, .b1) == 0)
                        try list.append(.{ .from = .d1, .to = .b1, .typ = .CastleKingside });
            } else {
                if (!b.o_pieces.check(.f1) and
                    !b.o_pieces.check(.g1) and
                    attack_count(b, .f1) == 0 and
                    attack_count(b, .g1) == 0)
                        try list.append(.{ .from = .e1, .to = .g1, .typ = .CastleKingside });
            }
        }
        if (b.castle.oq) {
            if (b.mir.horiz) {
                if (!b.o_pieces.check(.e1) and
                    !b.o_pieces.check(.f1) and
                    !b.o_pieces.check(.g1) and
                    attack_count(b, .e1) == 0 and
                    attack_count(b, .f1) == 0)
                        try list.append(.{ .from = .d1, .to = .f1, .typ = .CastleQueenside });
            } else {
                if (!b.o_pieces.check(.d1) and
                    !b.o_pieces.check(.c1) and
                    !b.o_pieces.check(.b1) and
                    attack_count(b, .d1) == 0 and
                    attack_count(b, .c1) == 0)
                        try list.append(.{ .from = .e1, .to = .c1, .typ = .CastleQueenside });
            }
        }
    }
}
