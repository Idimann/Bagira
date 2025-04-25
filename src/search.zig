const std = @import("std");
const tp = @import("types.zig");
const bo = @import("board.zig");
const mv = @import("movegen.zig");
const ev = @import("eval.zig");

fn better(f: SearchResult, s: SearchResult) bool {
    return f.val > s.val or (f.val == s.val and f.dep < s.dep);
}

fn better_eq(f: SearchResult, s: SearchResult) bool {
    return f.val > s.val or (f.val == s.val and f.dep <= s.dep);
}

pub const SearchResult = struct {
    val: f16,
    dep: u8,
};
const SearchMax: SearchResult = .{ .val = std.math.floatMax(f16), .dep = 0 };
const SearchMin: SearchResult = .{ .val = -std.math.floatMax(f16), .dep = 255 };

inline fn gameEnd(b: *const bo.Board, mvs: usize, checks: usize) ?f16 {
    // Check and stalemate
    if (mvs == 0) return if (checks > 0) -std.math.floatMax(f16) else 0;

    //50 move rule
    if (b.move_rule >= 100) return 0;

    if (b.history.items.len > 0) {
        const hash = b.hash();
        //Iterating in reverse should generally be more efficient
        var it = b.history.items.len - 1;
        while (true) {
            if (b.history.items[it] == hash) return 0;
            if (it > 0) it -= 1 else break;
        }
    }

    return null;
}

fn abSearch(
    b: *bo.Board,
    al: SearchResult,
    beta: SearchResult,
    stop: bool,
    alloc: std.mem.Allocator,
) !SearchResult {
    var alpha = al;

    var list = std.ArrayList(tp.Move).init(alloc);
    defer list.deinit();
    const checks = try if (stop) mv.gen_with_checks(b, &list) else mv.gen(b, &list);

    if (gameEnd(b, list.items.len, checks)) |val|
        return .{ .val = val, .dep = 0 }
    else if (stop and checks == 0)
        return .{ .val = ev.eval(b), .dep = 0 };

    var ret = SearchMin;
    for (list.items) |mov| {
        const undo = try b.apply(mov);

        const betNeg = .{ .val = -beta.val, .dep = 255 - beta.dep };
        const alNeg = .{ .val = -alpha.val, .dep = 255 - alpha.dep };

        var val = try abSearch(b, betNeg, alNeg, undo.typ == null, alloc);

        try b.remove(mov, undo);

        val.val = -val.val;
        val.dep += 1;
        if (better(val, ret))
            ret = val;

        if (better(ret, alpha))
            alpha = ret;
        if (better_eq(alpha, beta))
            break;
    }

    return ret;
}

var Random: std.rand.Xoshiro256 = undefined;

pub fn initRandom() !void {
    Random = std.rand.DefaultPrng.init(blk: {
        var seed: u64 = undefined;
        try std.posix.getrandom(std.mem.asBytes(&seed));
        break :blk seed;
    });
}

const CarloResult = struct {
    check: u8,
    explor: u8,
    result: i8,
    hash: u64,

    pub inline fn updateCheck(self: *CarloResult) void {
        self.check = self.explor ^ @as(u8, @bitCast(self.result));
    }
};
const CarloTableSize = (16 * (1 << 20)) / @sizeOf(CarloResult); //16 Mb
var CarloTable = std.mem.zeroes([CarloTableSize]CarloResult);

fn getCarloInsert(b: *const bo.Board) *CarloResult {
    const hash = b.hash() % CarloTableSize;
    const hash2 = b.hash2();
    const ret = &CarloTable[hash];

    if (ret.check != ret.explor ^ @as(u8, @bitCast(ret.result)) or ret.hash != hash2) {
        ret.explor = 0;
        ret.result = 0;
        ret.hash = hash2;
        ret.updateCheck();
    }
    return ret;
}

const CarloConstant = std.math.sqrt2;
pub fn carloSearch(b: *bo.Board, alloc: std.mem.Allocator) !f16 {
    var list = std.ArrayList(tp.Move).init(alloc);
    const checks = try mv.gen(b, &list);

    if (gameEnd(b, list.items.len, checks)) |val| {
        list.deinit();
        return val;
    }

    var move: ?tp.Move = null;

    var total: u8 = 0;
    var onlyZeroes = false;
    for (list.items) |mov| {
        const undo = try b.apply(mov);

        const info = getCarloInsert(b);
        total += info.explor;
        if (info.explor == 0) onlyZeroes = true;

        try b.remove(mov, undo);
    }

    var ret = -std.math.floatMax(f16);
    for (list.items) |mov| {
        const undo = try b.apply(mov);

        const info = getCarloInsert(b);
        if (onlyZeroes and info.explor > 0) {
            try b.remove(mov, undo);
            continue;
        }

        var val = (try abSearch(b, SearchMin, SearchMax, false, alloc)).val;
        //Avoiding three fold blindness
        val += @as(f16, @floatCast(Random.random().floatNorm(f64))) * 1e-6;

        const result = @as(f16, @floatFromInt(info.result));
        const explor = @as(f16, @floatFromInt(info.explor));
        const cmp: f16 = if (onlyZeroes) val else (val + result) / explor +
            CarloConstant * std.math.sqrt(@log(@as(f16, @floatFromInt(total))) / explor);

        if (cmp > ret) {
            move = mov;
            ret = cmp;
        }

        try b.remove(mov, undo);
    }

    if (move) |m| {
        list.deinit();
        const undo = try b.apply(m);
        const returning = -(try carloSearch(b, alloc));
        try b.remove(m, undo);

        const res = getCarloInsert(b);
        res.explor += 1;
        res.result += @intFromFloat(returning);
        res.updateCheck();

        return returning;
    }

    return error.NoMoveAvailable;
}
