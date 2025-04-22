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

fn genPawn(b: *const bo.Board, sq: tp.Square, list: *std.ArrayList(tp.Move)) !void {
    const combi = b.o_pieces.o(b.t_pieces);
    const north = sq.getApply(.North);

    // Main squares
    if (!combi.check(north)) {
        const move = tp.Move{ .from = sq, .to = north, .typ = .Normal };
        if (sq.rank() == .Rank2 and !combi.check(sq.getApply(.NorthNorth))) {
            const move2 =
                tp.Move{ .from = sq, .to = sq.getApply(.NorthNorth), .typ = .Normal };
            try list.appendSlice(&[_]tp.Move{ move, move2 });
        } else {
            try list.append(move);
        }
    }

    var hit = ta.PawnAttacks[@intFromEnum(sq)].a(b.t_pieces);
    while (hit.popLsb()) |to| {
        try list.append(tp.Move{ .from = sq, .to = to, .typ = .Normal });
    }

    const en_passant = ta.PawnAttacks[@intFromEnum(sq)]
        .a(b.pawns)
        .without(b.o_pieces)
        .without(b.t_pieces);
    if (en_passant.lsb()) |to| {
        try list.append(tp.Move{ .from = sq, .to = to, .typ = .EnPassant });
    }
}

fn genKing(b: *const bo.Board, sq: tp.Square, list: *std.ArrayList(tp.Move)) !void {
    var iter = ta.KingAttacks[@intFromEnum(sq)].without(b.o_pieces);
    while (iter.popLsb()) |to| {
        try list.append(tp.Move{ .from = sq, .to = to, .typ = .Normal });
    }
}

fn genKnight(b: *const bo.Board, sq: tp.Square, list: *std.ArrayList(tp.Move)) !void {
    var iter = ta.KnightAttacks[@intFromEnum(sq)].without(b.o_pieces);
    while (iter.popLsb()) |to| {
        try list.append(tp.Move{ .from = sq, .to = to, .typ = .Normal });
    }
}

fn genLine(b: *const bo.Board, sq: tp.Square, list: *std.ArrayList(tp.Move)) !void {
    const combi = b.o_pieces.o(b.t_pieces).a(ta.getLineMask(sq));

    var iter = ta.getLine(sq, combi).without(b.o_pieces);
    while (iter.popLsb()) |to| {
        try list.append(tp.Move{ .from = sq, .to = to, .typ = .Normal });
    }
}

fn genDiag(b: *const bo.Board, sq: tp.Square, list: *std.ArrayList(tp.Move)) !void {
    const combi = b.o_pieces.o(b.t_pieces).a(ta.getDiagMask(sq));

    var iter = ta.getDiag(sq, combi).without(b.o_pieces);
    while (iter.popLsb()) |to| {
        try list.append(tp.Move{ .from = sq, .to = to, .typ = .Normal });
    }
}

pub fn gen(b: *const bo.Board, list: *std.ArrayList(tp.Move)) !void {
    var check_count: usize = 0;

    check_count += ta.KnightAttacks[@intFromEnum(b.o_king)].a(b.t_pieces
        .without(b.pawns)
        .without(b.lines)
        .without(b.diags)
        .without(b.t_king.toBoard())).popcount();
    check_count += ta.PawnAttacks[@intFromEnum(b.o_king)].a(b.t_pieces
        .a(b.pawns)).popcount();

    var iter = b.o_pieces;
    while (iter.popLsb()) |sq| {
        if (b.o_king == sq)
            try genKing(b, sq, list)
        else if (b.pawns.check(sq))
            try genPawn(b, sq, list)
        else {
            const lin = b.lines.check(sq);
            const dia = b.diags.check(sq);

            if (lin) try genLine(b, sq, list);
            if (dia) try genDiag(b, sq, list);
            if (!lin and !dia) try genKnight(b, sq, list);
        }
    }
}
