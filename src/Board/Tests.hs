
{-# LANGUAGE TemplateHaskell #-} 

module Board.Tests where

import System.Random
import Test.QuickCheck

import TestLib
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
                        equalExcept board newBoard [pos]
  where
    newBoard = setB pos sq board
    
--------------------------------------------------------------------------------
-- Show and Read
--------------------------------------------------------------------------------

prop_showRead :: Board -> Bool
prop_showRead board = board == read (show board)

--------------------------------------------------------------------------------
-- applyMove
--------------------------------------------------------------------------------

prop_applyMoveNormalMove :: Pos -> Pos -> Board -> Property
prop_applyMoveNormalMove p1 p2 b = condition ==> result
    where
        condition = isOccupied (getB p1 b) && isOccupied (getB p1 b) && p1 /= p2
        result = equal && p1Empty && p2OldP1

        move = NormalMove p1 p2
        b' = applyMove move b

        equal = equalExcept b b' [p1, p2]
        p1Empty = isEmpty $ getB p1 b'
        p2OldP1 = getB p2 b' == getB p1 b

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

equalExcept :: Board -> Board -> [Pos] -> Bool
equalExcept b1 b2 ps = all (\p -> getB p b1 == getB p b2) $ otherPositions ps

otherPositions ps = [p | p <- allPositions, not $ p `elem` ps]

allPositions = [Pos row col | row <- [0..7], col <- [0..7]]

return []
runTests = $quickCheckAll
