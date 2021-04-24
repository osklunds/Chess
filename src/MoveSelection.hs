
module MoveSelection
( moveColor
)
where

import Data.Maybe

import Board
import Moves.CheckAware
import MoveSelection.Score
import Optimize


data State = State { board   :: Board
                   , reachBy :: Maybe ((Int,Int),(Int,Int))
                   , turn    :: Color }


-- depth must be 2 or larger in order to detect check and checkmate
moveColor :: Int -> Color -> Board -> ((Int,Int),(Int,Int))
moveColor depth color board = fromJust $ reachBy nextState
  where
    state     = State { board = board, reachBy = Nothing, turn = color }
    nextState = optimize genStates (evalState color) depth state

genStates :: State -> [State]
genStates (State {board = board, turn = color}) = states
  where
    moves  = movesForColor color board
    states = map (\move -> State { board   = applyMove move board,
                                   reachBy = Just move,
                                   turn    = invert color}) moves

evalState :: Color -> State -> Int
evalState color (State {board = board}) = scoreForColor color board

-- TODO: GameState should be extended with e.g. last moves (to detect 3 draw),
-- if player has move king and rooks, no capture for 50 turns etc, and let
-- move selection take this into account.