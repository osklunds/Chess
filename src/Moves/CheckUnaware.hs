
-- Generation of all syntactially possible moves.

module Moves.CheckUnaware
( movesForColor
)
where

import Board
import qualified Moves.NormalMoves as NM

movesForColor :: Color -> Board -> [Move]
movesForColor color board = map f $  NM.movesForColor color board
    where
        f ((rowS,colS),(rowD,colD)) = NormalMove (Pos rowS colS) (Pos rowD colD)

