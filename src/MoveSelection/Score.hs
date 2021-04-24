
module MoveSelection.Score
( scoreForColor
)
where

import Board
import GameResult


scoreForColor :: Color -> Board -> Int
scoreForColor color board
  | gameRes == Normal    = score
  | gameRes == Check     = score
  | gameRes == Checkmate = maxBound
  | gameRes == Draw      = 0
  where
    gameRes = gameResult (invert color) board
    score   = foldB (\sc sq -> sc + scoreForSquare color sq) 0 board

scoreForSquare :: Color -> Square -> Int
scoreForSquare _color Empty = 0
scoreForSquare color (Piece color' kind) = case kind of
                                              -- Kings are treated separately
                                              King   -> 0
                                              Queen  -> 10*multiplier
                                              Rook   -> 5*multiplier
                                              Bishop -> 3*multiplier
                                              Knight -> 3*multiplier
                                              Pawn   -> 1*multiplier
  where
    multiplier = if color == color' then 1 else -1