
module MoveSelection.Score
( scoreForColor
)
where

import Types
import GameResult

-- By keeping track of if found kings, a speed improvement was observed
data FoldState = FoldState { score :: Int
                           , foundKing :: Bool
                           , foundOtherKing :: Bool
                           }

-- TODO: Remove the need to check for game result. But first needs
-- many tests in MoveSelection
scoreForColor :: Color -> Color -> Board -> Int
scoreForColor color turn board
  | gameRes == Normal    = score
  | gameRes == Check     = score
  | gameRes == Checkmate = maxBound
  | gameRes == Draw      = 0
  where
    gameRes = gameResult turn board
    score   = calculateScore color board

calculateScore :: Color -> Board -> Int
calculateScore color board
  | not $ foundKing finalState      = minBound
  | not $ foundOtherKing finalState = maxBound
  | otherwise                       = score finalState
  where
    initState = FoldState { score = 0
                          , foundKing = False
                          , foundOtherKing = False }
    finalState = foldB (foldFun color) initState board

foldFun :: Color -> FoldState -> Square -> FoldState
foldFun color state (Piece color' King)
  | color == color' = state { foundKing = True }
  | color /= color' = state { foundOtherKing = True }
foldFun color state square = state { score = curScore + accScore }
  where
    curScore = scoreForSquare color square
    accScore = score state

scoreForSquare :: Color -> Square -> Int
scoreForSquare _color Empty = 0
scoreForSquare color (Piece color' kind) = case kind of
                                              -- Kings are treated separately
                                              Queen  -> 10*multiplier
                                              Rook   -> 5*multiplier
                                              Bishop -> 3*multiplier
                                              Knight -> 3*multiplier
                                              Pawn   -> 1*multiplier
  where
    multiplier = if color == color' then 1 else -1