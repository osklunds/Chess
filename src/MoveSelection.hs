
-- Selection of a move for a board using optimization. In other words,
-- applying Score and Moves with Optimize.

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module MoveSelection
( moveColor
)
where

import Data.Maybe
import Test.QuickCheck

import Types
import Moves
import Score
import Optimize


data State = State { board         :: Board
                   , reachBy       :: Maybe Move
                   , numberOfMoves :: Int
                   , turn          :: Color }
           deriving (Eq, Ord)

newtype Score = Score (Int, -- Numerical score
                       Int) -- Number of moves to reach
                deriving (Eq)


-- depth must be 2 or larger in order to detect check and checkmate
moveColor :: Int -> Color -> Board -> Move
moveColor depth color board = fromJust $ reachBy nextState
  where
    initialState = State { board         = board
                         , reachBy       = Nothing
                         , numberOfMoves = 0
                         , turn          = color }
    nextState    = optimize genStates (evalState color) depth initialState

genStates :: State -> [State]
genStates (State {board, turn, numberOfMoves}) = states
  where
    hasKing = anyB (== (Piece turn King)) board
    moves   = case hasKing of
                True  -> movesFun turn board
                False -> []
    -- Optimization. If "turn" has no king anyway, turn lost in a previous
    -- state and there's no need to check moves now.
    -- TODO: Move that to movesF

    -- TODO: Need in invariant in board that checks castlestate consistency, to
    -- detect if someone external doesn't use applyMove to update the board
    states  = map (\move -> State { board         = applyMove move board
                                  , reachBy       = Just move
                                  , numberOfMoves = numberOfMoves + 1
                                  , turn          = invert turn}) moves

-- TODO: Perhaps score fun should take state?
evalState :: Color -> State -> Score
evalState color (State {board, numberOfMoves, turn}) =
  Score (scoreForColor color turn board, numberOfMoves)

-- TODO: Need to make it more advanced so that
-- M1,M2 is better than M2,M1, where M1 is promote, and M2 is a dummy move
-- In the end, the order doesn't matter if just evaluating score and num moves
-- to reach there, but if they are equal, it should be better to get a higher
-- score earlier in the game. Or is it the second to last that should be
-- compared? A separate unit test (not necesarily about chess) to test this
-- might be needed.

-- The solution could be that store board score for each level/depth. The
-- score at the deepest depth is what is prio, but if that is equal,
-- compared the second deepest, and so on.

-- Or to use move evaluation in the optimization algorithm

-- The solution to go for, for now, is that if two moves have the same score in
-- the end, then the move with the highest score *first* wins, if equal then
-- second, and so on. The reasoning is that long term they are the same. So do
-- the shortest term good move because in the next round, you are allowed to
-- think one more step, and then it's good to start from a better board. A more
-- concrete reason is to force the computer to "make progress".

instance Ord Score where
  compare (Score (numScore1,numMoves1)) (Score (numScore2,numMoves2))
    = compare (numScore1,numMoves2) (numScore2,numMoves1)

instance Bounded Score where
  minBound = Score (minBound,maxBound)
  maxBound = Score (maxBound,minBound)

-- TODO: GameState should be extended with e.g. last moves (to detect 3 draw),
-- if player has move king and rooks, no capture for 50 turns etc, and let
-- move selection take this into account.
