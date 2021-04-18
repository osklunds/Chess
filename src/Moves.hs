
module Moves
( movesForColor
)
where

import Prelude as P

import Board as B
import qualified Moves.CheckUnawareMoves as CU


movesForColor :: Color -> Board -> [((Int,Int),(Int,Int))]
movesForColor color board = filter (isKingSafeAfterMove color board) allMoves
  where
    allMoves               = CU.movesForColor color board

isKingSafeAfterMove :: Color -> Board -> ((Int,Int),(Int,Int)) -> Bool
isKingSafeAfterMove color board move = P.any (/= Piece color King) destSquares
  where
    newBoard    = applyMove move board
    movesOther  = CU.movesForColor (otherColor color) newBoard
    dests       = P.map snd movesOther
    destSquares = P.map (\dest -> get dest newBoard) dests


