
{-# LANGUAGE TemplateHaskell #-} 

module Board.Tests where

import System.Random
import Test.QuickCheck

import Board as B

--------------------------------------------------------------------------------
-- get
--------------------------------------------------------------------------------

prop_getTopleft :: Bool
prop_getTopleft = getB (Pos 0 0) defaultBoard == Piece Black Rook

prop_getBottomRight :: Bool
prop_getBottomRight = getB (Pos 7 7) defaultBoard == Piece White Rook

prop_getMiddle :: Bool
prop_getMiddle = getB (Pos 4 4) defaultBoard == Empty

--------------------------------------------------------------------------------
-- set
--------------------------------------------------------------------------------

prop_set :: Pos -> Square -> Board -> Bool
prop_set pos sq board = getB pos newBoard == sq &&
                        equalExcept board newBoard pos
  where
    newBoard = setB pos sq board

equalExcept :: Board -> Board -> Pos -> Bool
equalExcept board1 board2 pos =
  all (\p -> getB p board1 == getB p board2) otherPositions
  where
    allPositions = [Pos row col | row <- [0..7], col <- [0..7]]
    otherPositions = filter ((/=) pos) allPositions

--------------------------------------------------------------------------------
-- Show and Read
--------------------------------------------------------------------------------

prop_showRead :: Board -> Bool
prop_showRead board = board == read (show board)

--------------------------------------------------------------------------------
-- applyMove
--------------------------------------------------------------------------------

prop_applyMoveNormalMove :: Int -> Board -> Bool
prop_applyMoveNormalMove seed board = undefined


randomPos :: (RandomGen g) => g -> Board -> (Pos, g)
randomPos g board = undefined


return []
runTests = $quickCheckAll
