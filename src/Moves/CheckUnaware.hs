
-- Generation of all syntactially possible moves.

module Moves.CheckUnaware
( movesForColor
)
where

import Board
import qualified Moves.NormalMoves as NM

movesForColor :: Color -> Board -> [Move]
movesForColor c b = normalMoves c b ++ promotes c b

normalMoves :: Color -> Board -> [Move]
normalMoves c b = map f $  NM.movesForColor c b
    where
        f ((rowS,colS),(rowD,colD)) = NormalMove (Pos rowS colS) (Pos rowD colD)

promotes :: Color -> Board -> [Move]
promotes c b = [Promote p k | p <- withPawn, k <- [Rook, Bishop, Knight, Queen]]
    where
        r = if c == White then 0 else 7
        ps = [Pos r c | c <- [0..7]]
        withPawn = [p | p <- ps, let atP = getB p b, isPawn atP, isColor c atP]



