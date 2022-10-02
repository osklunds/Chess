
-- Generation of all syntactially possible moves.

module Moves.Naive.CheckUnaware
( movesF
)
where

import Board
import qualified Moves.Naive.NormalMoves as NM
import Moves.Common


movesF :: MovesFun
movesF = concatApply [normalMoves, promotes, castlings]

concatApply :: [MovesFun] -> MovesFun
concatApply fs c b = concat [f c b | f <- fs]

normalMoves :: MovesFun
normalMoves = NM.movesF

promotes :: MovesFun
promotes c b = [Promote p k | p <- withPawn, k <- [Rook, Bishop, Knight, Queen]]
    where
        r = if c == White then 0 else 7
        ps = [Pos r c | c <- [0..7]]
        withPawn = [p | p <- ps, let atP = getB p b, isPawn atP, isColor c atP]

castlings :: MovesFun
castlings = concatApply [kingSideCastle, queenSideCastle]

kingSideCastle :: MovesFun
kingSideCastle c b
    | getBL [Pos row col | col <- [4..7]] b == lane = [Castle c KingSide]
    | otherwise                                     = []
    where
        row = homeRow c
        lane = [Piece c King, Empty, Empty, Piece c Rook]

queenSideCastle :: MovesFun
queenSideCastle c b
    | getBL [Pos row col | col <- [0..4]] b == lane = [Castle c QueenSide]
    | otherwise                                     = []
    where
        row = homeRow c
        lane = [Piece c Rook, Empty, Empty, Empty, Piece c King]

getBL :: [Pos] -> Board -> [Square]
getBL ps b = [getB p b | p <- ps]
