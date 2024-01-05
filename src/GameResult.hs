
-- Used for determining the result of a game.

-- TODO: Feels like this is partially duplicated from MoveSelection. Is this module/logic needed?

module GameResult
( Result(..) 
, gameResult
, isKingThreatened
)
where

import Types
import Moves as M
import Moves.Naive.CheckAware


data Result = Normal | Check | Checkmate | Draw
            deriving (Eq)

gameResult :: Color -> Board -> Result
gameResult player board = case (canMove, isThreatened) of
                            (True , True ) -> Check
                            (True , False) -> Normal
                            (False, True ) -> Checkmate
                            (False, False) -> Draw
  where
    canMove      = not $ null $ M.movesFun player board
    isThreatened = isKingThreatened player board
