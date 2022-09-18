
{-# LANGUAGE TemplateHaskell #-} 

module Board.Tests where

import System.Random
import Test.QuickCheck
import Data.Maybe

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

prop_applyNormalMove :: Pos -> Pos -> Board -> Property
prop_applyNormalMove src dst b = condition ==> result
    where
        condition = srcOccupied && dstOccupied && srcDstDiffer
        srcOccupied = isOccupied $ getB src b
        dstOccupied = isOccupied $ getB dst b
        srcDstDiffer = src /= dst

        result = equal && srcEmpty && dstIsOldSrc

        move = NormalMove src dst
        b' = fromJust $ applyMove move b

        equal = equalExcept b b' [src, dst]
        srcEmpty = isEmpty $ getB src b'
        dstIsOldSrc = getB dst b' == getB src b

prop_applyNormalMoveNoPieceAtSrc :: Pos -> Pos -> Board -> Property
prop_applyNormalMoveNoPieceAtSrc src dst b = condition ==> result
    where
        condition = srcEmpty && srcDstDiffer
        srcEmpty = isEmpty $ getB src b
        srcDstDiffer = src /= dst

        move = NormalMove src dst
        b' = applyMove move b

        result = isNothing b'



--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

equalExcept :: Board -> Board -> [Pos] -> Bool
equalExcept b1 b2 ps = all (\p -> getB p b1 == getB p b2) $ otherPositions ps

otherPositions ps = [p | p <- allPositions, not $ p `elem` ps]

allPositions = [Pos row col | row <- [0..7], col <- [0..7]]

return []
runTests = $quickCheckAll
