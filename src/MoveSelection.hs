
-- Selection of a move for a board using optimization. In other words,
-- using Optimize for Board.

{-# LANGUAGE NamedFieldPuns #-}

module MoveSelection
( moveColor
)
where

import Data.Maybe

import Board
import Moves.CheckAware
import MoveSelection.Score
import Optimize


data State = State { board         :: Board
                   , reachBy       :: Maybe ((Int,Int),(Int,Int))
                   , numberOfMoves :: Int
                   , turn          :: Color }

newtype Score = Score (Int, -- Numerical score
                       Int) -- Number of moves to reach
                deriving (Eq)


-- depth must be 2 or larger in order to detect check and checkmate
moveColor :: Int -> Color -> Board -> ((Int,Int),(Int,Int))
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
                True  -> movesForColor turn board
                False -> []
    -- Optimization. If "turn" has no king anyway, turn lost in a previous
    -- state and there's no need to check moves now.
    states  = map (\move -> State { board         = applyMove move board
                                  , reachBy       = Just move
                                  , numberOfMoves = numberOfMoves + 1
                                  , turn          = invert turn}) moves

-- TODO: Perhaps score fun should take state?
evalState :: Color -> State -> Score
evalState color (State {board, numberOfMoves, turn}) =
  Score (scoreForColor color turn board, numberOfMoves)

instance Ord Score where
  compare (Score (numScore1,numMoves1)) (Score (numScore2,numMoves2))
    = compare (numScore1,numMoves2) (numScore2,numMoves1)

instance Bounded Score where
  minBound = Score (minBound,maxBound)
  maxBound = Score (maxBound,minBound)


-- TODO: GameState should be extended with e.g. last moves (to detect 3 draw),
-- if player has move king and rooks, no capture for 50 turns etc, and let
-- move selection take this into account.