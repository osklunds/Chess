
-- Used for determining the result of a game.

module GameResult
( Result(..) 
, gameResult
, isKingThreatened
)
where

import Board
import Moves as M
import Moves.Naive.CheckUnaware as CU


data Result = Normal | Check | Checkmate | Draw
            deriving (Eq)

gameResult :: Color -> Board -> Result
gameResult player board = case (canMove, isThreatened) of
                            (True , True ) -> Check
                            (True , False) -> Normal
                            (False, True ) -> Checkmate
                            (False, False) -> Draw
  where
    canMove      = not $ null $ M.movesF player board
    isThreatened = isKingThreatened player board

isKingThreatened :: Color -> Board -> Bool
isKingThreatened color board = any (== Piece color King) destSquares
  where
    movesOther  = CU.movesF (invert color) board
    dests       = map moveToDest movesOther
    destSquares = map (\dest -> getB dest board) dests
