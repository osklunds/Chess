
-- Generation of all possible moves, when taking into account that
-- you may not be in check afterwards.

module Moves.Naive.CheckAware
( movesF
)
where

import Board
import qualified Moves.Naive.CheckUnaware as CU


movesF :: Color -> Board -> [Move]
movesF color board = filter (isKingSafeAfterMove color board) $
                            CU.movesF color board

isKingSafeAfterMove :: Color -> Board -> Move -> Bool
isKingSafeAfterMove color board move = all (/= Piece color King) destSquares
  where
    newBoard    = applyMove move board
    movesOther  = CU.movesF (invert color) newBoard
    dests       = map moveToDest movesOther
    destSquares = map (\dest -> getB dest newBoard) dests
