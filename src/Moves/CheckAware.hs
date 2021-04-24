
module Moves.CheckAware
( movesForColor
)
where

import Board
import qualified Moves.CheckUnaware as CU


movesForColor :: Color -> Board -> [((Int,Int),(Int,Int))]
movesForColor color board = filter (isKingSafeAfterMove color board) $
                                   CU.movesForColor color board

isKingSafeAfterMove :: Color -> Board -> ((Int,Int),(Int,Int)) -> Bool
isKingSafeAfterMove color board move = all (/= Piece color King) destSquares
  where
    newBoard    = applyMove move board
    movesOther  = CU.movesForColor (invert color) newBoard
    dests       = map snd movesOther
    destSquares = map (\dest -> getB dest newBoard) dests
