
module MoveSelection
( moveColor
)
where

import Prelude as P
import Data.Maybe

import Board as B
import Moves
import Score
import Optimize


data State = State { board   :: Board
                   , reachBy :: Maybe ((Int,Int),(Int,Int))
                   , turn    :: Color }


-- depth must be 2 or larger? To detect check and checkmate
moveColor :: Int -> Color -> Board -> ((Int,Int),(Int,Int))
moveColor depth color board = fromJust $ reachBy nextState
  where
    state     = State { board = board, reachBy = Nothing, turn = color }
    nextState = optimize genStates (evalState color) depth state

genStates :: State -> [State]
genStates (State {board = board, turn = color}) = states
  where
    moves  = movesForColor color board
    states = P.map (\move -> State { board   = applyMove move board,
                                     reachBy = Just move,
                                     turn    = otherColor color}) moves

evalState :: Color -> State -> Int
evalState color (State {board = board}) = scoreForColor color board