
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

prop_setCastleState :: Color -> CastleState -> CastleState -> Board -> Bool
prop_setCastleState color castleState1 castleState2 board =
    castleState1 == getCastleState color board'' &&
    castleState2 == getCastleState (invert color) board''
    where
        board' = setCastleState color castleState1 board
        board'' = setCastleState (invert color) castleState2 board'

--------------------------------------------------------------------------------
-- Show and Read
--------------------------------------------------------------------------------

prop_showRead :: Board -> Bool
prop_showRead board = board == read (show board)

prop_show :: Property
prop_show = counterexample (expString ++ "\n" ++ actString) result
    where
        castleState = (CastleState { leftRook = Moved, king = Unmoved, rightRook = Unmoved })
        board = setB (Pos 0 3) Empty $
                setB (Pos 6 6) (Piece Black Bishop) $
                setB (Pos 6 0) Empty $
                setCastleState Black castleState $
                defaultBoard
        expString = "  M       U     U\n\
                    \  a b c d e f g h\n\
                    \8 ♜ ♞ ♝   ♚ ♝ ♞ ♜ 8\n\
                    \7 ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟ 7\n\
                    \6                 6\n\
                    \5                 5\n\
                    \4                 4\n\
                    \3                 3\n\
                    \2   ♙ ♙ ♙ ♙ ♙ ♝ ♙ 2\n\
                    \1 ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖ 1\n\
                    \  a b c d e f g h\n\
                    \  U       U     U"
        actString = show board
        result = expString == actString

prop_read :: Property
prop_read = counterexample (show expBoard ++ "\n" ++ show actBoard) result
    where
        inputString = "  U       U     U\n\
                      \  a b c d e f g h  \n\
                      \8 ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜ 8\n\
                      \7 ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟ 7\n\
                      \6                 6\n\
                      \5         ♞       5\n\
                      \4                 4\n\
                      \3                 3\n\
                      \2 ♙ ♙ ♙ ♙   ♙ ♙ ♙ 2\n\
                      \1 ♜ ♘ ♗ ♕ ♔ ♗ ♘ ♖ 1\n\
                      \  a b c d e f g h\n\
                      \  U       M     U"
        actBoard = read inputString
        expCastleState = (CastleState { leftRook = Unmoved, king = Moved, rightRook = Unmoved })
        expBoard = setB (Pos 3 4) (Piece Black Knight) $
                   setB (Pos 7 0) (Piece Black Rook) $
                   setB (Pos 6 4) Empty $
                   setCastleState White expCastleState $
                   defaultBoard
        result = expBoard == actBoard

--------------------------------------------------------------------------------
-- applyMove - normal
--------------------------------------------------------------------------------

prop_applyNormalNoCapture :: Property
prop_applyNormalNoCapture = verifyBoardsEqual expBoardAfter actBoardAfter
    where
        boardBefore = read "  U       U     U  \n\
                           \  0 1 2 3 4 5 6 7  \n\
                           \0 ♜       ♚     ♜ 0\n\
                           \1 ♙         ♜ ♛   1\n\
                           \2       ♙ ♔ ♘ ♛   2\n\
                           \3   ♜ ♗ ♘   ♟   ♟ 3\n\
                           \4[♞]♘ ♜ ♙         4\n\
                           \5       ♘ ♘ ♞ ♘ ♛ 5\n\
                           \6  [ ]  ♛ ♘ ♙   ♟ 6\n\
                           \7 ♖             ♖ 7\n\
                           \  0 1 2 3 4 5 6 7  \n\
                           \  M       U     M"

        expBoardAfter = read "  U       U     U  \n\
                             \  0 1 2 3 4 5 6 7  \n\
                             \0 ♜       ♚     ♜ 0\n\
                             \1 ♙         ♜ ♛   1\n\
                             \2       ♙ ♔ ♘ ♛   2\n\
                             \3   ♜ ♗ ♘   ♟   ♟ 3\n\
                             \4[ ]♘ ♜ ♙         4\n\
                             \5       ♘ ♘ ♞ ♘ ♛ 5\n\
                             \6  [♞]  ♛ ♘ ♙   ♟ 6\n\
                             \7 ♖             ♖ 7\n\
                             \  0 1 2 3 4 5 6 7  \n\
                             \  M       U     M"

        actBoardAfter = applyMove (NormalMove (Pos 4 0) (Pos 6 1)) boardBefore

prop_applyNormalCapture :: Property
prop_applyNormalCapture = verifyBoardsEqual expBoardAfter actBoardAfter
    where
        boardBefore = read "  U       U     U  \n\
                           \  0 1 2 3 4 5 6 7  \n\
                           \0       ♕       ♝ 0\n\
                           \1 ♝ ♗ ♘         ♗ 1\n\
                           \2   ♜       ♔ ♚ ♝ 2\n\
                           \3    [♖]♜     ♘   3\n\
                           \4 ♕               4\n\
                           \5 ♛         ♙     5\n\
                           \6   ♛[♝]          6\n\
                           \7   ♗ ♛     ♗     7\n\
                           \  0 1 2 3 4 5 6 7  \n\
                           \  M       U     M"

        expBoardAfter = read "  U       U     U  \n\
                             \  0 1 2 3 4 5 6 7  \n\
                             \0       ♕       ♝ 0\n\
                             \1 ♝ ♗ ♘         ♗ 1\n\
                             \2   ♜       ♔ ♚ ♝ 2\n\
                             \3    [ ]♜     ♘   3\n\
                             \4 ♕               4\n\
                             \5 ♛         ♙     5\n\
                             \6   ♛[♖]          6\n\
                             \7   ♗ ♛     ♗     7\n\
                             \  0 1 2 3 4 5 6 7  \n\
                             \  M       U     M"
        actBoardAfter = applyMove (NormalMove (Pos 3 2) (Pos 6 2)) boardBefore

prop_applyMoveBlackLeftRookNormal :: Property
prop_applyMoveBlackLeftRookNormal = verifyBoardsEqual expBoardAfter actBoardAfter
    where
        boardBefore = read "  U       U     U  \n\
                           \  0 1 2 3 4 5 6 7  \n\
                           \0 ♜       ♚     ♜ 0\n\
                           \1             ♘   1\n\
                           \2       ♝ ♘ ♙     2\n\
                           \3         ♜     ♖ 3\n\
                           \4 ♖ ♞         ♘   4\n\
                           \5         ♛       5\n\
                           \6     ♜ ♛   ♟     6\n\
                           \7         ♔     ♖ 7\n\
                           \  0 1 2 3 4 5 6 7  \n\
                           \  U       M     M"

        expBoardAfter = read "  M       U     U  \n\
                             \  0 1 2 3 4 5 6 7  \n\
                             \0         ♚     ♜ 0\n\
                             \1             ♘   1\n\
                             \2       ♝ ♘ ♙     2\n\
                             \3         ♜     ♖ 3\n\
                             \4 ♜ ♞         ♘   4\n\
                             \5         ♛       5\n\
                             \6     ♜ ♛   ♟     6\n\
                             \7         ♔     ♖ 7\n\
                             \  0 1 2 3 4 5 6 7  \n\
                             \  U       M     M"

        actBoardAfter = applyMove (NormalMove (Pos 0 0) (Pos 4 0)) boardBefore

prop_applyMoveBlackRightRookNormal :: Property
prop_applyMoveBlackRightRookNormal = verifyBoardsEqual expBoardAfter actBoardAfter
    where
        boardBefore = read "  U       U     U  \n\
                           \  0 1 2 3 4 5 6 7  \n\
                           \0 ♚       ♛     ♜ 0\n\
                           \1     ♖ ♕         1\n\
                           \2   ♘ ♝       ♝   2\n\
                           \3   ♙   ♟       ♙ 3\n\
                           \4 ♘     ♖     ♞   4\n\
                           \5 ♔         ♜     5\n\
                           \6   ♜   ♗   ♘     6\n\
                           \7 ♛ ♞     ♝     ♝ 7\n\
                           \  0 1 2 3 4 5 6 7  \n\
                           \  M       M     U"

        expBoardAfter = read "  U       U     M  \n\
                             \  0 1 2 3 4 5 6 7  \n\
                             \0 ♚       ♛       0\n\
                             \1     ♖ ♕       ♜ 1\n\
                             \2   ♘ ♝       ♝   2\n\
                             \3   ♙   ♟       ♙ 3\n\
                             \4 ♘     ♖     ♞   4\n\
                             \5 ♔         ♜     5\n\
                             \6   ♜   ♗   ♘     6\n\
                             \7 ♛ ♞     ♝     ♝ 7\n\
                             \  0 1 2 3 4 5 6 7  \n\
                             \  M       M     U"

        actBoardAfter = applyMove (NormalMove (Pos 0 7) (Pos 1 7)) boardBefore

prop_applyMoveBlackKingNormal :: Property
prop_applyMoveBlackKingNormal = verifyBoardsEqual expBoardAfter actBoardAfter
    where
        boardBefore = read "  U       U     U\n\
                           \  0 1 2 3 4 5 6 7  \n\
                           \0   ♕     ♚     ♜ 0\n\
                           \1         ♙ ♗ ♟ ♘ 1\n\
                           \2   ♜   ♛ ♞ ♖ ♝   2\n\
                           \3 ♟   ♛   ♕       3\n\
                           \4 ♔ ♜   ♗       ♙ 4\n\
                           \5               ♞ 5\n\
                           \6   ♖ ♞       ♗   6\n\
                           \7 ♖               7\n\
                           \  0 1 2 3 4 5 6 7\n\
                           \  U       M     M"

        expBoardAfter = read "  U       M     U\n\
                             \  0 1 2 3 4 5 6 7  \n\
                             \0   ♕       ♚   ♜ 0\n\
                             \1         ♙ ♗ ♟ ♘ 1\n\
                             \2   ♜   ♛ ♞ ♖ ♝   2\n\
                             \3 ♟   ♛   ♕       3\n\
                             \4 ♔ ♜   ♗       ♙ 4\n\
                             \5               ♞ 5\n\
                             \6   ♖ ♞       ♗   6\n\
                             \7 ♖               7\n\
                             \  0 1 2 3 4 5 6 7\n\
                             \  U       M     M"

        actBoardAfter = applyMove (NormalMove (Pos 0 4) (Pos 0 5)) boardBefore

prop_applyMoveWhiteLeftRookNormal :: Property
prop_applyMoveWhiteLeftRookNormal = verifyBoardsEqual expBoardAfter actBoardAfter
    where
        boardBefore = read "  U       U     U  \n\
                           \  0 1 2 3 4 5 6 7  \n\
                           \0 ♜       ♚   ♜   0\n\
                           \1 ♖ ♗ ♙ ♖ ♛   ♟ ♞ 1\n\
                           \2           ♕     2\n\
                           \3 ♙ ♟ ♘ ♘ ♝       3\n\
                           \4   ♟ ♞   ♝   ♕   4\n\
                           \5           ♟     5\n\
                           \6 ♛ ♙ ♝     ♗ ♟ ♗ 6\n\
                           \7 ♖       ♔   ♗   7\n\
                           \  0 1 2 3 4 5 6 7  \n\
                           \  U       U     U"

        expBoardAfter = read "  U       U     U  \n\
                             \  0 1 2 3 4 5 6 7  \n\
                             \0 ♜       ♚   ♜   0\n\
                             \1 ♖ ♗ ♙ ♖ ♛   ♟ ♞ 1\n\
                             \2           ♕     2\n\
                             \3 ♙ ♟ ♘ ♘ ♝       3\n\
                             \4   ♟ ♞   ♝   ♕   4\n\
                             \5           ♟     5\n\
                             \6 ♖ ♙ ♝     ♗ ♟ ♗ 6\n\
                             \7         ♔   ♗   7\n\
                             \  0 1 2 3 4 5 6 7  \n\
                             \  M       U     U"
        actBoardAfter = applyMove (NormalMove (Pos 7 0) (Pos 6 0)) boardBefore

prop_applyMoveWhiteRightRookNormal :: Property
prop_applyMoveWhiteRightRookNormal = verifyBoardsEqual expBoardAfter actBoardAfter
    where
        boardBefore = read "  U       U     U  \n\
                           \  0 1 2 3 4 5 6 7  \n\
                           \0 ♞       ♚     ♜ 0\n\
                           \1     ♘     ♝   ♕ 1\n\
                           \2         ♞       2\n\
                           \3         ♕       3\n\
                           \4       ♗         4\n\
                           \5     ♕ ♝   ♜   ♞ 5\n\
                           \6 ♝   ♔     ♞   ♙ 6\n\
                           \7               ♖ 7\n\
                           \  0 1 2 3 4 5 6 7  \n\
                           \  M       M     U"

        expBoardAfter = read "  U       U     U  \n\
                             \  0 1 2 3 4 5 6 7  \n\
                             \0 ♞       ♚     ♜ 0\n\
                             \1     ♘     ♝   ♕ 1\n\
                             \2         ♞       2\n\
                             \3         ♕       3\n\
                             \4       ♗         4\n\
                             \5     ♕ ♝   ♜   ♞ 5\n\
                             \6 ♝   ♔     ♞   ♙ 6\n\
                             \7 ♖               7\n\
                             \  0 1 2 3 4 5 6 7  \n\
                             \  M       M     M"
        actBoardAfter = applyMove (NormalMove (Pos 7 7) (Pos 7 0)) boardBefore

prop_applyMoveWhiteKingNormal :: Property
prop_applyMoveWhiteKingNormal = verifyBoardsEqual expBoardAfter actBoardAfter
    where
        boardBefore = read "  U       U     U  \n\
                           \  0 1 2 3 4 5 6 7  \n\
                           \0 ♜   ♗   ♚     ♜ 0\n\
                           \1 ♟       ♟     ♕ 1\n\
                           \2   ♗ ♝ ♖ ♘   ♛   2\n\
                           \3   ♜         ♘   3\n\
                           \4 ♟   ♘           4\n\
                           \5   ♞ ♖     ♛ ♛   5\n\
                           \6   ♘ ♘ ♛ ♝ ♖   ♟ 6\n\
                           \7 ♖       ♔ ♜   ♖ 7\n\
                           \  0 1 2 3 4 5 6 7  \n\
                           \  U       U     U"

        expBoardAfter = read "  U       U     U  \n\
                             \  0 1 2 3 4 5 6 7  \n\
                             \0 ♜   ♗   ♚     ♜ 0\n\
                             \1 ♟       ♟     ♕ 1\n\
                             \2   ♗ ♝ ♖ ♘   ♛   2\n\
                             \3   ♜         ♘   3\n\
                             \4 ♟   ♘           4\n\
                             \5   ♞ ♖     ♛ ♛   5\n\
                             \6   ♘ ♘ ♛ ♝ ♖   ♟ 6\n\
                             \7 ♖         ♔   ♖ 7\n\
                             \  0 1 2 3 4 5 6 7  \n\
                             \  U       M     U"
        actBoardAfter = applyMove (NormalMove (Pos 7 4) (Pos 7 5)) boardBefore

--------------------------------------------------------------------------------
-- applyMove - castle
--------------------------------------------------------------------------------

prop_applyMoveBlackKingSideCastle :: Property
prop_applyMoveBlackKingSideCastle = verifyBoardsEqual expBoardAfter actBoardAfter
    where
        boardBefore = read "  U       U     U  \n\
                           \  0 1 2 3 4 5 6 7  \n\
                           \0 ♜       ♚     ♜ 0\n\
                           \1 ♜ ♝             1\n\
                           \2 ♟   ♞ ♟ ♛     ♖ 2\n\
                           \3   ♜     ♝       3\n\
                           \4         ♙       4\n\
                           \5   ♖   ♛ ♞       5\n\
                           \6   ♟ ♗ ♞     ♘   6\n\
                           \7 ♖   ♗   ♔     ♖ 7\n\
                           \  0 1 2 3 4 5 6 7  \n\
                           \  U       U     U"

        expBoardAfter = read "  U       M     M  \n\
                             \  0 1 2 3 4 5 6 7  \n\
                             \0 ♜         ♜ ♚   0\n\
                             \1 ♜ ♝             1\n\
                             \2 ♟   ♞ ♟ ♛     ♖ 2\n\
                             \3   ♜     ♝       3\n\
                             \4         ♙       4\n\
                             \5   ♖   ♛ ♞       5\n\
                             \6   ♟ ♗ ♞     ♘   6\n\
                             \7 ♖   ♗   ♔     ♖ 7\n\
                             \  0 1 2 3 4 5 6 7  \n\
                             \  U       U     U"
        actBoardAfter = applyMove (Castle Black KingSide) boardBefore

prop_applyMoveBlackQueenSideCastle :: Property
prop_applyMoveBlackQueenSideCastle = verifyBoardsEqual expBoardAfter actBoardAfter
    where
        boardBefore = read "  U       U     M  \n\
                           \  0 1 2 3 4 5 6 7  \n\
                           \0 ♜       ♚       0\n\
                           \1         ♖ ♗ ♙   1\n\
                           \2             ♗ ♕ 2\n\
                           \3   ♙   ♜ ♝       3\n\
                           \4     ♟     ♟     4\n\
                           \5         ♞ ♟ ♟ ♗ 5\n\
                           \6       ♕   ♗ ♝   6\n\
                           \7         ♔     ♖ 7\n\
                           \  0 1 2 3 4 5 6 7  \n\
                           \  M       U     U"

        expBoardAfter = read "  M       M     M  \n\
                             \  0 1 2 3 4 5 6 7  \n\
                             \0     ♚ ♜         0\n\
                             \1         ♖ ♗ ♙   1\n\
                             \2             ♗ ♕ 2\n\
                             \3   ♙   ♜ ♝       3\n\
                             \4     ♟     ♟     4\n\
                             \5         ♞ ♟ ♟ ♗ 5\n\
                             \6       ♕   ♗ ♝   6\n\
                             \7         ♔     ♖ 7\n\
                             \  0 1 2 3 4 5 6 7  \n\
                             \  M       U     U"
        actBoardAfter = applyMove (Castle Black QueenSide) boardBefore

prop_applyMoveWhiteKingSideCastle :: Property
prop_applyMoveWhiteKingSideCastle = verifyBoardsEqual expBoardAfter actBoardAfter
    where
        boardBefore = read "  M       M     M  \n\
                           \  0 1 2 3 4 5 6 7  \n\
                           \0   ♕             0\n\
                           \1   ♗     ♘ ♟ ♜   1\n\
                           \2 ♙ ♕   ♚ ♞ ♕ ♛   2\n\
                           \3 ♘   ♛ ♟     ♙ ♛ 3\n\
                           \4         ♜     ♜ 4\n\
                           \5 ♛         ♛   ♜ 5\n\
                           \6   ♙ ♖   ♕ ♙     6\n\
                           \7   ♘     ♔     ♖ 7\n\
                           \  0 1 2 3 4 5 6 7  \n\
                           \  M       U     U"

        expBoardAfter = read "  M       M     M  \n\
                             \  0 1 2 3 4 5 6 7  \n\
                             \0   ♕             0\n\
                             \1   ♗     ♘ ♟ ♜   1\n\
                             \2 ♙ ♕   ♚ ♞ ♕ ♛   2\n\
                             \3 ♘   ♛ ♟     ♙ ♛ 3\n\
                             \4         ♜     ♜ 4\n\
                             \5 ♛         ♛   ♜ 5\n\
                             \6   ♙ ♖   ♕ ♙     6\n\
                             \7   ♘       ♖ ♔   7\n\
                             \  0 1 2 3 4 5 6 7  \n\
                             \  M       M     M"
        actBoardAfter = applyMove (Castle White KingSide) boardBefore

prop_applyMoveWhiteQueenSideCastle :: Property
prop_applyMoveWhiteQueenSideCastle = verifyBoardsEqual expBoardAfter actBoardAfter
    where
        boardBefore = read "  M       M     M  \n\
                           \  0 1 2 3 4 5 6 7  \n\
                           \0 ♜   ♕   ♚ ♖ ♗ ♗ 0\n\
                           \1 ♖   ♝   ♗ ♞   ♗ 1\n\
                           \2 ♜   ♘   ♗   ♗ ♟ 2\n\
                           \3 ♜ ♞     ♗ ♟     3\n\
                           \4   ♟     ♘ ♖   ♛ 4\n\
                           \5 ♕             ♘ 5\n\
                           \6   ♜   ♞       ♛ 6\n\
                           \7 ♖       ♔     ♘ 7\n\
                           \  0 1 2 3 4 5 6 7  \n\
                           \  U       U     U"

        expBoardAfter = read "  M       M     M  \n\
                             \  0 1 2 3 4 5 6 7  \n\
                             \0 ♜   ♕   ♚ ♖ ♗ ♗ 0\n\
                             \1 ♖   ♝   ♗ ♞   ♗ 1\n\
                             \2 ♜   ♘   ♗   ♗ ♟ 2\n\
                             \3 ♜ ♞     ♗ ♟     3\n\
                             \4   ♟     ♘ ♖   ♛ 4\n\
                             \5 ♕             ♘ 5\n\
                             \6   ♜   ♞       ♛ 6\n\
                             \7     ♔ ♖       ♘ 7\n\
                             \  0 1 2 3 4 5 6 7  \n\
                             \  M       M     U"
        actBoardAfter = applyMove (Castle White QueenSide) boardBefore

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

verifyBoardsEqual :: Board -> Board -> Property
verifyBoardsEqual exp act = counterexample errorString result
    where
        errorString = "Expected:\n" ++ show exp ++ "\n\nActual:\n" ++ show act
        result = exp == act

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
