
-- Selection of a move for a board using optimization. In other words,
-- applying Score and Moves with Optimize.

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module MoveSelection
( selectMove
)
where

import Data.Maybe
import Test.QuickCheck
import Debug.Trace

import Types
import Moves
import Score
import Optimize
import qualified Optimize.Types

--------------------------------------------------------------------------------
-- "The great composition"
--------------------------------------------------------------------------------

data State = State { board :: Board
                   , prevStates :: [State]
                   , reachBy :: Maybe Move }
           deriving (Eq, Ord, Show)

-- depth must be 2 or larger in order to detect check and checkmate
-- TODO: Is the above comment really true?
selectMove :: Int -> Board -> Move
selectMove depth board = fromJust $ reachBy nextState
  where
    initialState = State { board
                         , prevStates = []
                         , reachBy = Nothing }
    nextState = optimize genStates evalState depth initialState

--------------------------------------------------------------------------------
-- State generation
--------------------------------------------------------------------------------

genStates :: Optimize.Types.GenFun State
genStates currentState = states
    where
        (State {board, prevStates}) = currentState

        turn = getTurn board
        hasKing = anyB (== (Piece turn King)) board
        moves = case hasKing of
                    True  -> movesFun board
                    False -> []
        -- Optimization. If "turn" has no king anyway, turn lost in a previous
        -- state and there's no need to check moves now.
        -- TODO: Move that to movesF

        -- TODO: Need in invariant in board that checks castlestate consistency, to
        -- detect if someone external doesn't use applyMove to update the board
        states = map (\move -> State { board = applyMove move board
                                     , prevStates = currentState:prevStates
                                     , reachBy = Just move }) moves

--------------------------------------------------------------------------------
-- Score
--------------------------------------------------------------------------------

-- TODO: This code works, but it really needs some cleanup

data StateScore = StateScoreMax |
                  StateScoreMin |
                  StateScore State
                  deriving (Show)

evalState :: Optimize.Types.EvalFun State StateScore
evalState state = StateScore state

instance Bounded StateScore where
  minBound = StateScoreMin
  maxBound = StateScoreMax

instance Eq StateScore where
    _ == _ = error "There should be no need for equality checks"

instance Ord StateScore where
    compare StateScoreMin StateScoreMin = EQ
    compare StateScoreMin _             = LT
    compare _             StateScoreMin = GT

    compare StateScoreMax StateScoreMax = EQ
    compare StateScoreMax _             = GT
    compare _             StateScoreMax = LT

    compare state1 state2 = if resultFinalStates /= EQ
                                then resultFinalStates
                                else comparePreviousScores state1 state2
        where
            resultFinalStates = compareStates state1 state2

-- TODO: Can solve test coverage for "edge cases" below here, by generating
-- random boards and if it crashes, it tests that code branch. So comment out
-- some code branches, run random boards, and when crashes, save that board.
-- Can also check decisions, like should the list be reversed? Find a board
-- where it makes a difference (crash here) and investigate that board.

compareStates :: StateScore -> StateScore -> Ordering
compareStates (StateScore state1) (StateScore state2) =
    compareScoreLists [scoreOfState state1]
                      [scoreOfState state2]
compareStates s1 s2 = error $
    "Two non StateScores are being compared" ++ show s1 ++ show s2

comparePreviousScores :: StateScore -> StateScore -> Ordering
comparePreviousScores (StateScore state1) (StateScore state2) =
    compareScoreLists prevScores1 prevScores2
    where
        makePrevScores :: State -> [Int]
        makePrevScores (State { prevStates }) =
            reverse $ map scoreOfState prevStates

        prevScores1 = makePrevScores state1
        prevScores2 = makePrevScores state2

scoreOfState :: State -> Int
scoreOfState = score . board

compareScoreLists :: [Int] -> [Int] -> Ordering
compareScoreLists [] [] = EQ
-- So far the score lists were equal, now, favor the shorter one
compareScoreLists [] _  = GT
compareScoreLists _ []  = LT
compareScoreLists (s1:rest1) (s2:rest2) = if result /= EQ
                                              then result
                                              else compareScoreLists rest1 rest2
    where
        result = compare s1 s2
        
-- Old notes

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

