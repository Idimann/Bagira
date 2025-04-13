const std = @import("std");
const types = @import("types.zig");

pub const Board = struct {
    o_pieces: types.BitBoard,
    t_pieces: types.BitBoard,
    pawns: types.BitBoard,
    diags: types.BitBoard,
    lines: types.BitBoard,
    o_king: types.Square,
    t_king: types.Square,

    white: bool,
    mirrored_h: bool,

    pub fn mirror(self: *Board) *Board {

    }
};
