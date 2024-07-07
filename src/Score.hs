
module Score
( score
, Result(..)
)
where

import Types
import Moves
import Moves.Naive.CheckAware (threatensKing)

-- By keeping track of if found kings, a speed improvement was observed
data FoldState = FoldState { scoreValue :: Int
                           , foundBlackKing :: Bool
                           , foundWhiteKing :: Bool
                           }
                 
data Result = Normal Bool
            | Draw
            | Checkmate
            deriving (Eq, Show)

score :: Board -> (Int, Result, [Move])
score board = (score, result, moves)
    where
        moves = movesFun board
        canMove = not $ null $ moves
        isThreatened = threatensKing $ invertTurn board
        scoreValue = calculateScore board
        -- isThreatened is expensive, so include it as a field that
        -- only is lazily calculated if needed, i.e. by Cli
        (score, result) = case (canMove, isThreatened) of
                            (True, _) -> (scoreValue, Normal isThreatened)
                            (False, False) -> (0, Draw)
                            (False, True) -> (if getTurn board == Black
                                                  then minBound
                                                  else maxBound, Checkmate)

-- Positive means Black is leading, negative means White is leading
-- (Talk about controversy :D)
calculateScore :: Board -> Int
calculateScore board
   -- Test all combinations of black and white king existing
   -- TODO: Should never happen that not 1 of each king. Assert
   -- and fix board generation
   | not $ foundBlackKing finalState = minBound
   | not $ foundWhiteKing finalState = maxBound
   | otherwise                       = scoreValue finalState
   where
       initState = FoldState { scoreValue = 0
                             , foundBlackKing = False
                             , foundWhiteKing = False }
       finalState = foldB foldFun initState board

foldFun :: FoldState -> Square -> FoldState
foldFun state (Piece color King)
  | color == Black = state { foundBlackKing = True }
  | color == White = state { foundWhiteKing = True }
foldFun state square = state { scoreValue = curScore + accScore }
  where
    curScore = scoreForSquare square
    accScore = scoreValue state

scoreForSquare :: Square -> Int
scoreForSquare  Empty = 0
scoreForSquare (Piece color kind) = case kind of
                                        -- Kings are treated separately
                                        Queen  -> 10*multiplier
                                        Rook   -> 5*multiplier
                                        Bishop -> 3*multiplier
                                        Knight -> 3*multiplier
                                        Pawn   -> 1*multiplier
    where
        multiplier = case color of
                        Black -> 1
                        White -> -1
