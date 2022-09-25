
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

prop_applyCastleWhiteKingSide :: Board -> Bool
prop_applyCastleWhiteKingSide = applyCastleKingSide White row
    where
        row = 7

prop_applyCastleBlackKingSide :: Board -> Bool
prop_applyCastleBlackKingSide = applyCastleKingSide Black row
    where
        row = 0

prop_applyCastleWhiteQueenSide :: Board -> Bool
prop_applyCastleWhiteQueenSide = applyCastleQueenSide White row
    where
        row = 7

prop_applyCastleBlackQueenSide :: Board -> Bool
prop_applyCastleBlackQueenSide = applyCastleQueenSide Black row
    where
        row = 0

applyCastleKingSide :: Color -> Int -> Board -> Bool
applyCastleKingSide = applyCastle KingSide rookC newRookC newKingC
    where
        rookC = 7
        newRookC = 5
        newKingC = 6

applyCastleQueenSide :: Color -> Int -> Board -> Bool
applyCastleQueenSide = applyCastle QueenSide rookC newRookC newKingC
    where
        rookC = 0
        newRookC = 3
        newKingC = 2

applyCastle :: Side -> Int -> Int -> Int -> Color -> Int -> Board -> Bool
applyCastle side rookC newRookC newKingC color row board =
    emptyAtOldKingP && kingAtNewP && emptyAtOldRookP && rookAtNewP && restSame
    where
        kingC = 4
        kingP = Pos row kingC
        newRookP = Pos row newRookC
        newKingP = Pos row newKingC
        rookP = Pos row rookC

        leftmostC = min rookC kingC
        rightmostC = max rookC kingC
        emptyPs = [Pos row col | col <- [leftmostC..rightmostC]]

        board' = setB kingP (Piece color King) $
                 setB rookP (Piece color Rook) $
                 setEmpty emptyPs $
                 removeKing color board
        
        move = Castle color side
        board'' = applyMove move board'

        emptyAtOldKingP = isEmpty $ getB kingP board''
        kingAtNewP = getB newKingP board'' == Piece color King
        emptyAtOldRookP = isEmpty $ getB rookP board''
        rookAtNewP = getB newRookP board'' == Piece color Rook
        restSame = equalExcept board' board'' [kingP, newRookP, newKingP, rookP]

removeKing :: Color -> Board -> Board
removeKing color = mapB f
    where
        f square = if square == Piece color King then Empty else square

setEmpty :: [Pos] -> Board -> Board
setEmpty [] b = b
setEmpty (p:ps) b = setEmpty ps $ setB p Empty b





        

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
