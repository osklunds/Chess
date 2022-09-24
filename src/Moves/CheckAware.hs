
-- Generation of all possible moves, when taking into account that
-- you may not be in check afterwards.

module Moves.CheckAware
( movesForColor
)
where

import Board
import qualified Moves.CheckUnaware as CU


movesForColor :: Color -> Board -> [Move]
movesForColor color board = filter (isKingSafeAfterMove color board) $
                                   CU.movesForColor color board

isKingSafeAfterMove :: Color -> Board -> Move -> Bool
isKingSafeAfterMove color board move = all (/= Piece color King) destSquares
  where
    newBoard    = applyMove move board
    movesOther  = CU.movesForColor (invert color) newBoard
    dests       = map moveToDest movesOther
    destSquares = map (\dest -> getB dest newBoard) dests

moveToDest :: Move -> Pos
moveToDest (NormalMove _src dst) = dst
moveToDest (Promote pos _kind) = pos
