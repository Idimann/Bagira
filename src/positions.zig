pub const start = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

pub const Mate = struct {
    pub const _1 = "7k/4Q3/6K1/8/8/8/8/8 w - - 0 1";
    pub const _2 = "7k/4Q3/4K3/8/8/8/8/8 w - - 0 1";
    pub const _4 = "7k/8/8/3K4/4Q3/8/8/8 w - - 0 1";
};

pub const Openings = struct {
    pub const caro_kann = "r2qkbnr/pp1nppp1/2p5/3pPbBp/3P3P/8/PPP2PP1/RN1QKBNR w KQkq - 0 1";
    pub const queen_gambit = "rnbqkbnr/ppp1pppp/8/3p4/2PP4/8/PP2PPPP/RNBQKBNR b KQkq - 0 1";
    pub const king_pawn = "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 1";
};

pub const Middlegames = struct {
    pub const spanish =
        "r3k2r/1bppqppp/p1n2n2/1p2p1B1/1b2P3/1BNP1N2/PPP1QPPP/R3K2R b KQkq - 0 1";
};

pub const Endgames = struct {};
