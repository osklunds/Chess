
{-# LANGUAGE TemplateHaskell #-} 

module Types.Board.Tests where

import System.Random
import Test.QuickCheck
import Data.Maybe
import Test.QuickCheck.Arbitrary

import Types.Board
import Types.Pos
import Types.Square
import Types.Move
import TestLib
import Moves.Naive.CheckUnaware

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

prop_setNonKingNonPawn :: Pos -> Square -> Board -> Property
prop_setNonKingNonPawn pos sq board = condition ==> setSquare pos sq board
    where
        condition = not (isKing sq) && not (isPawn sq)

prop_setKing :: Color -> Pos -> Board -> Bool
prop_setKing col pos board = setSquare pos sq noKingsBoard
    where
        sq = Piece col King
        noKingsBoard = mapB (\sq -> if isKing sq then Empty else sq) board

prop_setPawn :: Color -> Pos -> Board -> Property
prop_setPawn col pos@(Pos row _col) board = condition ==> setSquare pos sq board
    where
        sq = Piece col Pawn
        condition = row /= 0 && row /= 7

setSquare :: Pos -> Square -> Board -> Bool
setSquare pos sq board = newIsChanged && othersAreUnchanged
    where
        newBoard = setB pos sq board
        newIsChanged = getB pos newBoard == sq
        othersAreUnchanged = equalExcept board newBoard [pos]

--------------------------------------------------------------------------------
-- Show and Read
--------------------------------------------------------------------------------

prop_showRead :: Board -> Bool
prop_showRead board = board == read (show board)

prop_show :: Property
prop_show = counterexample (expString ++ "\n" ++ actString) result
    where
        board = setB (Pos 0 3) Empty $
                setB (Pos 6 6) (Piece Black Bishop) $
                setB (Pos 6 0) Empty $
                defaultBoard
        expString = "  a b c d e f g h\n\
                    \8 ♜ ♞ ♝   ♚ ♝ ♞ ♜ 8\n\
                    \7 ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟ 7\n\
                    \6                 6\n\
                    \5                 5\n\
                    \4                 4\n\
                    \3                 3\n\
                    \2   ♙ ♙ ♙ ♙ ♙ ♝ ♙ 2\n\
                    \1 ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖ 1\n\
                    \  a b c d e f g h"
        actString = show board
        result = expString == actString

prop_read :: Bool
prop_read = expBoard == read inputString
    where
        inputString = "  a b c d e f g h  \n\
                      \8 ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜ 8\n\
                      \7 ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟ 7\n\
                      \6                 6\n\
                      \5         ♞       5\n\
                      \4                 4\n\
                      \3                 3\n\
                      \2 ♙ ♙ ♙ ♙   ♙ ♙ ♙ 2\n\
                      \1 ♜ ♘ ♗ ♕ ♔ ♗ ♘ ♖ 1\n\
                      \  a b c d e f g h"
        expBoard = setB (Pos 3 4) (Piece Black Knight) $
                   setB (Pos 7 0) (Piece Black Rook) $
                   setB (Pos 6 4) Empty $
                   defaultBoard

--------------------------------------------------------------------------------
-- applyMove
--------------------------------------------------------------------------------

prop_applyNormalMove :: TwoDifferentPos -> Board -> Property
prop_applyNormalMove (TwoDifferentPos src dst) b = condition ==> result
    where
        condition = srcOccupied && dstOccupied && notPawnToEdge
        srcOccupied = isOccupied atSrc
        dstOccupied = isOccupied atDst
        notPawnToEdge = not ((isPawn atSrc) && (dstRow /= 0 || dstRow /= 7))

        atSrc = getB src b
        atDst = getB dst b
        (Pos dstRow _col) = dst

        move = NormalMove src dst
        b' = applyMove move b

        result = equal && srcEmpty && dstIsOldSrc
        equal = equalExcept b b' [src, dst]
        srcEmpty = isEmpty $ getB src b'
        dstIsOldSrc = getB dst b' == getB src b

-- TODO: Remove or rework
-- prop_applyPromote :: Pos -> Color -> Kind -> Board -> Property
-- prop_applyPromote p color kind b = condition ==> result
--     where
--         (Pos row col) = p
--         row' = if row <= 3
--                 then 1
--                 else 6
--         p' = (Pos row' col)

--         condition = atTopOrBottom && notToPawnOrKing
--         atTopOrBottom = row' == 1 && color == White ||
--                         row' == 6 && color == Black
--         notToPawnOrKing = kind /= Pawn && kind /= King

--         b' = setB p' (Piece color Pawn) b
--         move = Promote p' kind
--         b'' = applyMove move b'

--         result = equal && pHasNew
--         equal = equalExcept b' b'' [p']
--         pHasNew = getB p' b'' == Piece color kind

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

--------------------------------------------------------------------------------
-- Arbitrary
--------------------------------------------------------------------------------

prop_oneOfEachKing :: Board -> Bool
prop_oneOfEachKing b = foldB f (0,0) b == (1,1)
    where
        f (numBlack, numWhite) sq =
                case sq of
                    (Piece Black King) -> (numBlack + 1, numWhite)
                    (Piece White King) -> (numBlack, numWhite + 1)
                    _pos               -> (numBlack, numWhite)

prop_promoteDistribution :: Board -> Property
prop_promoteDistribution = moveDistribution pred
    where
        pred (Promote _src _dst _kind) = True
        pred _otherMove = False

prop_castleDistribution :: Board -> Property
prop_castleDistribution = moveDistribution pred
    where
        pred = (`elem` [Castle Black QueenSide, Castle Black KingSide])

moveDistribution :: (Move -> Bool) -> Board -> Property
moveDistribution pred b = collect moveAvailable True
    where
        moves = movesFun Black b
        moveAvailable = any (pred) moves


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
