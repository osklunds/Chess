
-- Generation of all syntactially possible moves.

module Moves.Naive.CheckUnaware
( movesF
)
where

import Board
import qualified Moves.Naive.NormalMoves as NM
import Moves.Common


movesF :: MovesFun
movesF c b = normalMoves c b ++ promotes c b ++ castlings c b

normalMoves :: Color -> Board -> [Move]
normalMoves c b = map f $  NM.movesF c b
    where
        f ((rowS,colS),(rowD,colD)) = NormalMove (Pos rowS colS) (Pos rowD colD)

promotes :: Color -> Board -> [Move]
promotes c b = [Promote p k | p <- withPawn, k <- [Rook, Bishop, Knight, Queen]]
    where
        r = if c == White then 0 else 7
        ps = [Pos r c | c <- [0..7]]
        withPawn = [p | p <- ps, let atP = getB p b, isPawn atP, isColor c atP]

castlings :: Color -> Board -> [Move]
castlings c b = kingSideCastle c b ++ queenSideCastle c b

kingSideCastle :: Color -> Board -> [Move]
kingSideCastle c b
    | getBL [Pos row col | col <- [4..7]] b == lane = [Castle c KingSide]
    | otherwise                                     = []
    where
        row = homeRow c
        lane = [Piece c King, Empty, Empty, Piece c Rook]

queenSideCastle :: Color -> Board -> [Move]
queenSideCastle c b
    | getBL [Pos row col | col <- [0..4]] b == lane = [Castle c QueenSide]
    | otherwise                                     = []
    where
        row = homeRow c
        lane = [Piece c Rook, Empty, Empty, Empty, Piece c King]

homeRow :: Color -> Int
homeRow White = 7
homeRow Black = 0

getBL :: [Pos] -> Board -> [Square]
getBL ps b = [getB p b | p <- ps]
