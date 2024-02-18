
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
import Debug.Trace

import Types
import Moves
import Score
import Optimize

--------------------------------------------------------------------------------
-- State
--------------------------------------------------------------------------------

data State = State { board :: Board
                   , prevStates :: [State]
                   , reachBy :: Maybe Move
                   , turn    :: Color }
           deriving (Eq, Ord, Show)

-- depth must be 2 or larger in order to detect check and checkmate
moveColor :: Int -> Color -> Board -> Move
moveColor depth color board = fromJust $ reachBy nextState
  where
    initialState = State { board
                         , prevStates = []
                         , reachBy = Nothing
                         , turn    = color }
    nextState = optimize genStates (evalState color) depth initialState

genStates :: State -> [State]
genStates currentState = states
    where
        (State {board, prevStates, turn}) = currentState

        hasKing = anyB (== (Piece turn King)) board
        moves = case hasKing of
                    True  -> movesFun turn board
                    False -> []
        -- Optimization. If "turn" has no king anyway, turn lost in a previous
        -- state and there's no need to check moves now.
        -- TODO: Move that to movesF

        -- TODO: Need in invariant in board that checks castlestate consistency, to
        -- detect if someone external doesn't use applyMove to update the board
        states = map (\move -> State { board = applyMove move board
                                     , prevStates = currentState:prevStates
                                     , reachBy = Just move
                                     , turn    = invert turn}) moves

--------------------------------------------------------------------------------
-- Score
--------------------------------------------------------------------------------

-- TODO: This code works, but it really needs some cleanup

evalState :: Color -> State -> Score
evalState color state = ScoreState color state

data Score = ScoreMax | ScoreMin | ScoreState Color State deriving (Eq, Show)

instance Bounded Score where
  minBound = ScoreMin
  maxBound = ScoreMax


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

-- TODO: Add test coverage where number of moves matter
-- TODO: Can solve test coverage for "edge cases" below here, by generating
-- random boards and if it crashes, it tests that code branch. So comment out
-- some code branches, run random boards, and when crashes, save that board.
-- Can also check decisions, like should the list be reversed? Find a board
-- where it makes a difference (crash here) and investigate that board.
instance Ord Score where
    compare ScoreMin ScoreMin = EQ
    compare ScoreMin _        = LT
    compare _        ScoreMin = GT

    compare ScoreMax ScoreMax = EQ
    compare ScoreMax _        = GT
    compare _        ScoreMax = LT

    compare state1 state2
        | result == EQ = comparePreviousScores state1 state2
        | otherwise    = result
        where
            result = compareFinalScores state1 state2

compareFinalScores :: Score -> Score -> Ordering
compareFinalScores (ScoreState color state1) (ScoreState _color state2) =
    compare (scoreForColorInState color state1)
            (scoreForColorInState color state2)
compareFinalScores s1 s2 =
    trace (show s1 ++ show s2) EQ

scoreForColorInState :: Color -> State -> Int
scoreForColorInState color state = scoreForColor color (turn state) (board state)

comparePreviousScores :: Score -> Score -> Ordering
comparePreviousScores (ScoreState color state1) (ScoreState _color state2) =
    compareScoreLists prevScores1 prevScores2
    where
        makePrevScores :: State -> [Int]
        makePrevScores state = reverse $ map (scoreForColorInState color) $ prevStates state

        prevScores1 = makePrevScores state1
        prevScores2 = makePrevScores state2

        compareScoreLists :: [Int] -> [Int] -> Ordering
        compareScoreLists [] [] = EQ
        compareScoreLists [] _  = GT
        compareScoreLists _ []  = LT
        compareScoreLists (s1:rest1) (s2:rest2)
            | result == EQ = compareScoreLists rest1 rest2
            | otherwise = result
            where
                result = compare s1 s2
        



    
       -- 1. Score in final board
       -- 2. Score in 1st, 2nd, 3rd etc board
       -- 3. If one chain runs out of boards, then the shorter wins

