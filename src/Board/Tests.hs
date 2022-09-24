
{-# LANGUAGE TemplateHaskell #-} 

module Board.Tests where

import System.Random
import Test.QuickCheck
import Data.Maybe
import Test.QuickCheck.Arbitrary

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

prop_applyNormalMove :: TwoDifferentPos -> Board -> Property
prop_applyNormalMove (TwoDifferentPos src dst) b = condition ==> result
    where
        condition = srcOccupied && dstOccupied
        srcOccupied = isOccupied $ getB src b
        dstOccupied = isOccupied $ getB dst b

        move = NormalMove src dst
        b' = applyMove move b

        result = equal && srcEmpty && dstIsOldSrc
        equal = equalExcept b b' [src, dst]
        srcEmpty = isEmpty $ getB src b'
        dstIsOldSrc = getB dst b' == getB src b

prop_applyPromote :: Pos -> Color -> Kind -> Board -> Property
prop_applyPromote p color kind b = condition ==> result
    where
        (Pos row col) = p
        row' = if row <= 3
                then 0
                else 7
        p' = (Pos row' col)

        condition = atTopOrBottom && notToPawnOrKing
        atTopOrBottom = row' == 0 && color == White ||
                        row' == 7 && color == Black
        notToPawnOrKing = kind /= Pawn && kind /= King

        b' = setB p' (Piece color Pawn) b
        move = Promote p' kind
        b'' = applyMove move b'

        result = equal && pHasNew
        equal = equalExcept b' b'' [p']
        pHasNew = getB p' b'' == Piece color kind







        

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

equalExcept :: Board -> Board -> [Pos] -> Bool
equalExcept b1 b2 ps = all (\p -> getB p b1 == getB p b2) $ otherPositions ps

otherPositions ps = [p | p <- allPositions, not $ p `elem` ps]

allPositions = [Pos row col | row <- [0..7], col <- [0..7]]

data TwoDifferentPos = TwoDifferentPos Pos Pos
                     deriving (Show)

instance Arbitrary TwoDifferentPos where
    arbitrary = arbitraryTwoDifferentPos

arbitraryTwoDifferentPos = do
    p1 <- arbitrary
    p2 <- arbitrary
    case p1 == p2 of
        True -> arbitraryTwoDifferentPos
        False -> return $ TwoDifferentPos p1 p2

return []
runTests = $quickCheckAll
