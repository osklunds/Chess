
{-# LANGUAGE TemplateHaskell #-}

module Moves.Naive.Tests where

import Data.List
import Test.QuickCheck

import Types
import Moves.Naive.CheckAware
import qualified Moves.Naive.CheckUnaware as CU
import Moves.Common
import Lib

--------------------------------------------------------------------------------
-- General
--------------------------------------------------------------------------------

prop_board1 :: Property
prop_board1 = verifyMoves expMoves Black board
  where
    board = read  "  M       U     U  \n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \0               ♝ 0\n\
                  \1 ♟ ♞           ♝ 1\n\
                  \2   ♔     ♝       2\n\
                  \3     ♜   ♙     ♙ 3\n\
                  \4     ♛   ♖ ♟ ♗ ♚ 4\n\
                  \5     ♖         ♜ 5\n\
                  \6 ♟     ♘ ♞ ♟ ♝ ♟ 6\n\
                  \7   ♞   ♜   ♞ ♖ ♝ 7\n\
                  \  0 1 2 3 4 5 6 7\n\
                  \  U       M     U"
    expMoves = -- King at (4 7)
               (normalMovesFrom (Pos 4 7) [(Pos 3 6), (Pos 4 6)]) ++
               -- Note that (Pos 3 7) and (Pos 6 4) are missing as destinations

               -- Queen at (4 2)
               (normalMovesFrom (Pos 4 2) [(Pos 4 3), (Pos 4 4), (Pos 5 2),
                                           (Pos 5 1), (Pos 4 1),
                                           (Pos 4 0), (Pos 3 1), (Pos 2 0),
                                           (Pos 3 3), (Pos 5 3)
                                          ]) ++

               -- Bishop at (0 7)
               (normalMovesFrom (Pos 0 7) [(Pos 1 6), (Pos 2 5), (Pos 3 4)]) ++

               -- Bishop at (1 7)
               (normalMovesFrom (Pos 1 7) [(Pos 2 6), (Pos 3 5), (Pos 4 4),
                                           (Pos 0 6)
                                          ]) ++

               -- Bishop at (2 4)
               (normalMovesFrom (Pos 2 4) [(Pos 1 5), (Pos 0 6), (Pos 3 5),
                                           (Pos 4 6), (Pos 3 3), (Pos 1 3),
                                           (Pos 0 2)
                                          ]) ++

               -- Bishop at (6 6)
               (normalMovesFrom (Pos 6 6) [(Pos 5 5), (Pos 4 4)]) ++

               -- Pawn at (1 0)
               (normalMovesFrom (Pos 1 0) [(Pos 2 0), (Pos 3 0), (Pos 2 1)]) ++

               -- Pawn at (6 7)
               promotesFrom (Pos 6 7) [(Pos 7 6)] ++

               -- Pawn at (6 5)
               promotesFrom (Pos 6 5) [(Pos 7 6)] ++

               -- Pawn at (6 0)
               promotesFrom (Pos 6 0) [(Pos 7 0)] ++

               -- Pawn at (4 5)
               (normalMovesFrom (Pos 4 5) [(Pos 5 5)]) ++

               -- Rook at (7 3)
               (normalMovesFrom (Pos 7 3) [(Pos 7 4), (Pos 7 2), (Pos 6 3)]) ++

               -- Rook at (5 7)
               (normalMovesFrom (Pos 5 7) [(Pos 5 6), (Pos 5 5), (Pos 5 4),
                                           (Pos 5 3), (Pos 5 2)
                                          ]) ++

               -- Rook at (3 2)
               (normalMovesFrom (Pos 3 2) [(Pos 2 2), (Pos 1 2), (Pos 0 2),
                                           (Pos 3 3), (Pos 3 4), (Pos 3 1),
                                           (Pos 3 0)
                                          ]) ++

               -- Knight at (1 1)
               (normalMovesFrom (Pos 1 1) [(Pos 0 3), (Pos 2 3), (Pos 3 0)]) ++

               -- Knight at (7 1)
               (normalMovesFrom (Pos 7 1) [(Pos 5 0), (Pos 5 2), (Pos 6 3)]) ++

               -- Knight at (7 5)
               (normalMovesFrom (Pos 7 5) [(Pos 5 6), (Pos 5 4), (Pos 6 3)]) ++

               -- Knight at (6 4)
               (normalMovesFrom (Pos 6 4) [(Pos 4 3), (Pos 5 6), (Pos 7 6),
                                           (Pos 7 2), (Pos 5 2)
                                          ])

prop_blackAndWhiteGiveSameMoves :: Board -> Bool
prop_blackAndWhiteGiveSameMoves board =
    blackMoves `listEq` mirroredWhiteMoves
    where
        blackMoves         = movesFun Black board
        swappedColors      = swapColors board
        mirroredBoard      = mirrorBoard swappedColors
        whiteMoves         = movesFun White mirroredBoard
        mirroredWhiteMoves = map mirrorMove whiteMoves

swapColors :: Board -> Board
swapColors = mapB swapColor

swapColor :: Square -> Square
swapColor Empty = Empty
swapColor (Piece White kind) = Piece Black kind
swapColor (Piece Black kind) = Piece White kind

mirrorBoard :: Board -> Board
mirrorBoard board = board'''
    where
        board' = foldl mirrorBoardAtPos
                       board
                       [Pos row col | row <- [0..3], col <- [0..7]]
        board'' = setCastleState Black (getCastleState White board) board'
        board''' = setCastleState White (getCastleState Black board) board''

mirrorBoardAtPos :: Board -> Pos -> Board
mirrorBoardAtPos board pos = swapPiecesAtPositions board pos mirroredPos
    where
        mirroredPos   = mirrorPos pos

mirrorPos :: Pos -> Pos
mirrorPos (Pos row col) = Pos (7 - row) col

mirrorMove :: Move -> Move
mirrorMove (NormalMove src dst) = NormalMove (mirrorPos src) (mirrorPos dst)
mirrorMove (Promote src dst kind) = Promote (mirrorPos src) (mirrorPos dst) kind
mirrorMove (Castle color side) = Castle (invert color) side

-- To test that the test implementation is correct
prop_mirrorBoard :: Property
prop_mirrorBoard = counterexample (show actMirroredBoard)
                                  (expMirroredBoard == actMirroredBoard)
    where
         board = read            "  M       U     U  \n\
                                 \  0 1 2 3 4 5 6 7  \n\
                                 \0 ♜ ♜           ♜ 0\n\
                                 \1   ♛ ♙   ♗   ♞ ♚ 1\n\
                                 \2                 2\n\
                                 \3           ♕ ♗   3\n\
                                 \4   ♖ ♙   ♗ ♞     4\n\
                                 \5   ♛   ♟   ♟   ♜ 5\n\
                                 \6       ♝ ♝ ♗ ♜ ♘ 6\n\
                                 \7 ♖     ♜ ♔       7\n\
                                 \  0 1 2 3 4 5 6 7  \n\
                                 \  U       M     U"

         expMirroredBoard = read "  U       M     U  \n\
                                 \  0 1 2 3 4 5 6 7  \n\
                                 \0 ♖     ♜ ♔       0\n\
                                 \1       ♝ ♝ ♗ ♜ ♘ 1\n\
                                 \2   ♛   ♟   ♟   ♜ 2\n\
                                 \3   ♖ ♙   ♗ ♞     3\n\
                                 \4           ♕ ♗   4\n\
                                 \5                 5\n\
                                 \6   ♛ ♙   ♗   ♞ ♚ 6\n\
                                 \7 ♜ ♜           ♜ 7\n\
                                 \  0 1 2 3 4 5 6 7  \n\
                                 \  M       U     U"

         actMirroredBoard = mirrorBoard board

prop_noNonMoves :: Color -> Board -> Bool
prop_noNonMoves color board = all isMove moves
    where
        moves = movesFun color board

        isMove (NormalMove src dst) = src /= dst
        isMove (Promote src dst _kind) = src /= dst
        isMove (Castle _color _side) = True

prop_srcAndDstAreWithinBoard :: Color -> Board -> Bool
prop_srcAndDstAreWithinBoard color board = all isMoveWithinBoard moves
    where
        moves = movesFun color board

        isWithinBoard (Pos row col) = 0 <= row && row < 8 && 0 <= col && col < 8

        isMoveWithinBoard (NormalMove src dst) = isWithinBoard src && isWithinBoard dst
        isMoveWithinBoard (Promote src dst _kind) = isWithinBoard src && isWithinBoard dst
        isMoveWithinBoard (Castle _color _side) = True

prop_dstIsNotSameColor :: Color -> Board -> Bool
prop_dstIsNotSameColor color board = all dstIsNotSameColor moves
    where
        moves = movesFun color board

        isNotSameColor pos = not (isColor color (getB pos board))

        dstIsNotSameColor (NormalMove _src dst) = isNotSameColor dst
        dstIsNotSameColor (Promote _src dst _kind) = isNotSameColor dst
        dstIsNotSameColor (Castle _color _side) = True

prop_srcIsSameColor :: Color -> Board -> Bool
prop_srcIsSameColor color board = all srcIsSameColor moves
    where
        moves = movesFun color board

        isSameColor pos = isColor color (getB pos board)

        srcIsSameColor (NormalMove src _dst) = isSameColor src
        srcIsSameColor (Promote src _dst _kind) = isSameColor src
        srcIsSameColor (Castle _color _side) = True

--------------------------------------------------------------------------------
-- Kind specific
--
-- These tests focus on boards with interesting scenarios for the different
-- kinds of pieces. However, moves for all pieces are still verified to
-- increase the total coverage and to test scenarios that are
-- randomly/accidentally created.
--------------------------------------------------------------------------------

prop_pawns :: Property
prop_pawns = verifyMoves expMoves Black board
  where
    board = read  "  M       U     U  \n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \0 ♚             ♔ 0\n\
                  \1   ♟     ♟   ♗ ♟ 1\n\
                  \2       ♖   ♗     2\n\
                  \3 ♟ ♕ ♖           3\n\
                  \4   ♙ ♝     ♗     4\n\
                  \5 ♜       ♛   ♘ ♞ 5\n\
                  \6   ♗ ♟ ♟     ♟   6\n\
                  \7     ♜     ♘   ♖ 7\n\
                  \  0 1 2 3 4 5 6 7\n\
                  \  U       U     U"
    expMoves = -- Pawn at (3 0)
               (normalMovesFrom (Pos 3 0) [(Pos 4 0), (Pos 4 1)]) ++

               -- Pawn at (1 1)
               (normalMovesFrom (Pos 1 1) [(Pos 2 1)]) ++

               -- Pawn at (6 2)
               [] ++

               -- Pawn at (6 3)
               (promotesFrom (Pos 6 3) [(Pos 7 3)]) ++

               -- Pawn at (1 4)
               (normalMovesFrom (Pos 1 4) [(Pos 2 4), (Pos 3 4), (Pos 2 3),
                                           (Pos 2 5)]) ++

               -- Pawn at (6 6)
               (promotesFrom (Pos 6 6) [(Pos 7 5), (Pos 7 6), (Pos 7 7)]) ++

               -- Pawn at (1 7)
               (normalMovesFrom (Pos 1 7) [(Pos 2 7), (Pos 3 7)]) ++

               -- Rook at (5 0)
               (normalMovesFrom (Pos 5 0) [(Pos 4 0), (Pos 5 1), (Pos 5 2),
                                           (Pos 5 3), (Pos 6 0), (Pos 7 0)]) ++
               
               -- Rook at (7 2)
               (normalMovesFrom (Pos 7 2) [(Pos 7 1), (Pos 7 0), (Pos 7 3),
                                           (Pos 7 4), (Pos 7 5)]) ++

               -- Queen at (5 4)
               (normalMovesFrom (Pos 5 4) [(Pos 5 3), (Pos 5 2), (Pos 5 1),
                                           (Pos 4 3), (Pos 3 2), (Pos 4 4),
                                           (Pos 3 4), (Pos 2 4), (Pos 4 5),
                                           (Pos 5 5), (Pos 5 6), (Pos 6 5),
                                           (Pos 7 6), (Pos 6 4), (Pos 7 4)]) ++
               -- Knight at (5 7)
               (normalMovesFrom (Pos 5 7) [(Pos 3 6), (Pos 4 5), (Pos 6 5),
                                           (Pos 7 6)]) ++

               -- Bishop at (4 2)
               (normalMovesFrom (Pos 4 2) [(Pos 3 1), (Pos 3 3), (Pos 2 4),
                                           (Pos 1 5), (Pos 0 6), (Pos 5 3),
                                           (Pos 6 4), (Pos 7 5), (Pos 5 1),
                                           (Pos 6 0)]) ++
               
               -- King at (0 0)
               (normalMovesFrom (Pos 0 0) [(Pos 0 1), (Pos 1 0)])

prop_knights :: Property
prop_knights = verifyMoves expMoves Black board
  where
    board = read  "  U       U     U  \n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \0 ♗       ♚     ♜ 0\n\
                  \1     ♝   ♜ ♜   ♙ 1\n\
                  \2 ♜     ♗     ♕ ♝ 2\n\
                  \3 ♖           ♖   3\n\
                  \4 ♟     ♘ ♞       4\n\
                  \5       ♖   ♘   ♝ 5\n\
                  \6 ♟ ♙         ♖   6\n\
                  \7 ♗   ♞   ♔     ♖ 7\n\
                  \  0 1 2 3 4 5 6 7\n\
                  \  U       U     U"
    expMoves = -- Knight at (4 4)
               (normalMovesFrom (Pos 4 4) [(Pos 2 5), (Pos 3 6), (Pos 5 6),
                                           (Pos 6 5), (Pos 6 3), (Pos 5 2),
                                           (Pos 3 2), (Pos 2 3)]) ++
               
               -- Knight at (7 2)
               (normalMovesFrom (Pos 7 2) [(Pos 5 1), (Pos 5 3), (Pos 6 4)]) ++

               -- Rook at (2 0)
               (normalMovesFrom (Pos 2 0) [(Pos 1 0), (Pos 0 0), (Pos 2 1),
                                           (Pos 2 2), (Pos 2 3), (Pos 3 0)]) ++
               
               -- Pawn at (4 0)
               (normalMovesFrom (Pos 4 0) [(Pos 5 0)]) ++

               -- Pawn at (6 0)
               [] ++

               -- Bishop at (1 2)
               (normalMovesFrom (Pos 1 2) [(Pos 0 1), (Pos 0 3), (Pos 2 3),
                                           (Pos 2 1), (Pos 3 0)]) ++
               
               -- King at (0 4)
               (normalMovesFrom (Pos 0 4) [(Pos 0 5), (Pos 0 3), (Pos 1 3)]) ++

               -- Rook at (1 4)
               (normalMovesFrom (Pos 1 4) [(Pos 1 3), (Pos 2 4), (Pos 3 4)]) ++

               -- Rook at (1 5)
               [] ++
               
               -- Rook at (0 7)
               (normalMovesFrom (Pos 0 7) [(Pos 0 6), (Pos 0 5), (Pos 1 7)]) ++

               -- Bishop at (2 7)
               (normalMovesFrom (Pos 2 7) [(Pos 1 6), (Pos 0 5), (Pos 3 6)]) ++

               -- Bishop at (5 7)
               (normalMovesFrom (Pos 5 7) [(Pos 6 6), (Pos 4 6), (Pos 3 5),
                                           (Pos 2 4), (Pos 1 3), (Pos 0 2)])

-- TODO: Add tests for rook, queen, bishop too

--------------------------------------------------------------------------------
-- Check and king
--------------------------------------------------------------------------------

prop_allKindsPreventKingMove :: Property
prop_allKindsPreventKingMove = verifyMoves expMoves White board
  where
    board = read  "  U       U     U  \n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \0                 0\n\
                  \1   ♛       ♝     1\n\
                  \2                 2\n\
                  \3   ♞             3\n\
                  \4   ♟       ♚     4\n\
                  \5       ♔         5\n\
                  \6 ♜     ♙         6\n\
                  \7                 7\n\
                  \  0 1 2 3 4 5 6 7\n\
                  \  U       U     U"
    expMoves = normalMovesFrom (Pos 5 3) [(Pos 6 4)]

prop_kingMovesAwayIfChecked :: Property
prop_kingMovesAwayIfChecked = verifyMoves expMoves White board
  where
    board = read  "  U       U     U  \n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \0                 0\n\
                  \1 ♟ ♟ ♟ ♙ ♟ ♟ ♟ ♟ 1\n\
                  \2                 2\n\
                  \3                 3\n\
                  \4                 4\n\
                  \5 ♜     ♔         5\n\
                  \6                 6\n\
                  \7               ♚ 7\n\
                  \  0 1 2 3 4 5 6 7\n\
                  \  U       U     U"
    expMoves = normalMovesFrom (Pos 5 3) [(Pos 4 2), (Pos 4 3), (Pos 4 4),
                                          (Pos 6 2), (Pos 6 3), (Pos 6 4)]

prop_blocksWithOtherIfKingIsChecked :: Property
prop_blocksWithOtherIfKingIsChecked = verifyMoves expMoves White board
  where
    board = read  "  U       U     U  \n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \0 ♚     ♘         0\n\
                  \1 ♟ ♟ ♟ ♟ ♟ ♟ ♙ ♟ 1\n\
                  \2                 2\n\
                  \3               ♛ 3\n\
                  \4     ♖       ♜ ♝ 4\n\
                  \5 ♜             ♔ 5\n\
                  \6       ♙     ♜ ♝ 6\n\
                  \7               ♛ 7\n\
                  \  0 1 2 3 4 5 6 7\n\
                  \  U       U     U"
    expMoves = (normalMovesFrom (Pos 6 3) [(Pos 5 3)]) ++
               (normalMovesFrom (Pos 4 2) [(Pos 5 2)])

prop_capturesThreatIfKingIsChecked :: Property
prop_capturesThreatIfKingIsChecked = verifyMoves expMoves White board
  where
    board = read  "  U       U     U  \n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \0 ♚   ♘     ♗     0\n\
                  \1               ♙ 1\n\
                  \2               ♛ 2\n\
                  \3               ♛ 3\n\
                  \4             ♜ ♝ 4\n\
                  \5 ♜             ♔ 5\n\
                  \6             ♜ ♝ 6\n\
                  \7               ♛ 7\n\
                  \  0 1 2 3 4 5 6 7\n\
                  \  U       U     U"
    expMoves = normalMovesFrom (Pos 0 5) [(Pos 5 0)]

prop_kingCanCaptureThreat :: Property
prop_kingCanCaptureThreat = verifyMoves expMoves White board
  where
    board = read  "  U       U     U  \n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \0 ♚   ♘     ♗     0\n\
                  \1               ♙ 1\n\
                  \2           ♝     2\n\
                  \3                 3\n\
                  \4                 4\n\
                  \5             ♜ ♔ 5\n\
                  \6                 6\n\
                  \7             ♝   7\n\
                  \  0 1 2 3 4 5 6 7\n\
                  \  U       U     U"
    expMoves = normalMovesFrom (Pos 5 7) [(Pos 5 6)]

prop_checkedMultipleTypesOfMoves :: Property
prop_checkedMultipleTypesOfMoves = verifyMoves expMoves White board
  where
    board = read  "  U       U     U  \n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \0 ♚   ♘     ♗     0\n\
                  \1               ♙ 1\n\
                  \2                 2\n\
                  \3                 3\n\
                  \4                 4\n\
                  \5             ♜ ♔ 5\n\
                  \6           ♙     6\n\
                  \7                 7\n\
                  \  0 1 2 3 4 5 6 7\n\
                  \  U       U     U"
    expMoves = (normalMovesFrom (Pos 5 7) [(Pos 5 6), (Pos 4 7), (Pos 6 7)]) ++
               (normalMovesFrom (Pos 6 5) [(Pos 5 6)])

prop_kingPinned :: Property
prop_kingPinned = verifyMoves expMoves White board
  where
    board = read  "  U       U     U  \n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \0                 0\n\
                  \1                 1\n\
                  \2       ♚         2\n\
                  \3                 3\n\
                  \4                 4\n\
                  \5   ♜   ♗   ♔     5\n\
                  \6                 6\n\
                  \7                 7\n\
                  \  0 1 2 3 4 5 6 7\n\
                  \  U       U     U"
    expMoves = (normalMovesFrom (Pos 5 5) [(Pos 5 4), (Pos 4 4), (Pos 4 5),
                                           (Pos 4 6), (Pos 5 6), (Pos 6 6),
                                           (Pos 6 5), (Pos 6 4)])

prop_movesAreSubsetOfCheckUnawareMoves :: Color -> Board -> Bool
prop_movesAreSubsetOfCheckUnawareMoves color board =
  moves `isSubsetOf` movesCheckUnaware
  where
    moves             = movesFun color board
    movesCheckUnaware = CU.movesFun color board

--------------------------------------------------------------------------------
-- Castling
--------------------------------------------------------------------------------

prop_noCastlingSinceKingWouldPassThreat :: Property
prop_noCastlingSinceKingWouldPassThreat = verifyCastlingMoves expMoves White board
    where
        board = read  "  U       U     U  \n\
                      \  0 1 2 3 4 5 6 7  \n\
                      \0   ♜ ♚     ♖ ♘   0\n\
                      \1 ♕   ♞ ♘ ♟ ♝ ♘   1\n\
                      \2       ♞ ♛       2\n\
                      \3   ♖ ♖   ♝     ♟ 3\n\
                      \4       ♜     ♞   4\n\
                      \5   ♙       ♜     5\n\
                      \6           ♖     6\n\
                      \7 ♖       ♔     ♖ 7\n\
                      \  0 1 2 3 4 5 6 7\n\
                      \  U       U     U"
        expMoves = [Castle White KingSide]

prop_noCastlingSinceLeftRookMoved :: Property
prop_noCastlingSinceLeftRookMoved = verifyCastlingMoves expMoves White board .&&.
                                    verifyCastlingMoves expMoves' White board'
    where
        board = read  "  U       U     U  \n\
                      \  0 1 2 3 4 5 6 7  \n\
                      \0 ♜         ♚   ♜ 0\n\
                      \1 ♟   ♘     ♗ ♘   1\n\
                      \2   ♕   ♙ ♗   ♟ ♞ 2\n\
                      \3 ♘ ♗         ♙   3\n\
                      \4         ♞   ♟   4\n\
                      \5   ♗     ♗ ♙     5\n\
                      \6       ♙     ♘   6\n\
                      \7 ♖       ♔     ♖ 7\n\
                      \  0 1 2 3 4 5 6 7\n\
                      \  M       U     U"
        expMoves = [Castle White KingSide]

        -- Sanity check
        board' = setCastleState White allCastleAvailableState board
        expMoves' = expMoves ++ [Castle White QueenSide]

allCastleAvailableState = (CastleState { leftRook = Unmoved,
                                          king = Unmoved,
                                          rightRook = Unmoved
                                        })

prop_noCastlingSinceKingIsChecked :: Property
prop_noCastlingSinceKingIsChecked = verifyCastlingMoves expMoves White board
    where
        board = read  "  U       U     U  \n\
                      \  0 1 2 3 4 5 6 7  \n\
                      \0 ♜         ♚   ♜ 0\n\
                      \1 ♟   ♘     ♗ ♘   1\n\
                      \2   ♕   ♙ ♗   ♟ ♞ 2\n\
                      \3 ♘ ♗         ♙   3\n\
                      \4         ♞   ♟   4\n\
                      \5   ♗     ♗ ♙     5\n\
                      \6       ♙   ♝ ♘   6\n\
                      \7 ♖       ♔     ♖ 7\n\
                      \  0 1 2 3 4 5 6 7\n\
                      \  U       U     U"
        expMoves = []

prop_noCastlingSinceDstAttacked :: Property
prop_noCastlingSinceDstAttacked = verifyCastlingMoves expMoves Black board
    where
        board = read  "  U       U     U  \n\
                      \  0 1 2 3 4 5 6 7  \n\
                      \0 ♜       ♚     ♜ 0\n\
                      \1           ♝     1\n\
                      \2   ♛     ♝   ♖   2\n\
                      \3 ♙           ♝   3\n\
                      \4   ♖ ♛ ♝     ♝ ♔ 4\n\
                      \5 ♙     ♞         5\n\
                      \6   ♟   ♜ ♘ ♕     6\n\
                      \7   ♖ ♜   ♗     ♜ 7\n\
                      \  0 1 2 3 4 5 6 7\n\
                      \  U       U     U"
        expMoves = [Castle Black QueenSide]

prop_bothCastlingsPossible :: Property
prop_bothCastlingsPossible = verifyCastlingMoves expMoves Black board
    where
        board = read  "  U       U     U  \n\
                      \  0 1 2 3 4 5 6 7  \n\
                      \0 ♜       ♚     ♜ 0\n\
                      \1     ♟ ♟ ♜ ♝     1\n\
                      \2 ♝   ♕     ♗   ♙ 2\n\
                      \3       ♝         3\n\
                      \4 ♟   ♝ ♝ ♝       4\n\
                      \5 ♘ ♙   ♖ ♙       5\n\
                      \6         ♙ ♔     6\n\
                      \7   ♜   ♞         7\n\
                      \  0 1 2 3 4 5 6 7\n\
                      \  U       U     U"
        expMoves = [Castle Black QueenSide, Castle Black KingSide]

prop_castlingKingSide :: Property
prop_castlingKingSide = verifyCastlingMoves expMoves Black board
    where
        board = read  "  U       U     U  \n\
                      \  0 1 2 3 4 5 6 7  \n\
                      \0 ♜   ♘   ♚     ♜ 0\n\
                      \1   ♞ ♛       ♝ ♟ 1\n\
                      \2       ♔   ♛ ♖   2\n\
                      \3   ♜ ♛ ♝     ♞ ♕ 3\n\
                      \4 ♟   ♕ ♞   ♜ ♟ ♟ 4\n\
                      \5       ♕     ♞   5\n\
                      \6   ♞   ♝     ♜ ♘ 6\n\
                      \7                 7\n\
                      \  0 1 2 3 4 5 6 7\n\
                      \  U       U     U"
        expMoves = [Castle Black KingSide]

prop_castlingQueenSide :: Property
prop_castlingQueenSide = verifyCastlingMoves expMoves Black board
    where
        board = read  "  U       U     U  \n\
                      \  0 1 2 3 4 5 6 7  \n\
                      \0 ♜       ♚       0\n\
                      \1 ♟ ♟ ♟ ♟ ♜ ♝     1\n\
                      \2         ♞ ♕     2\n\
                      \3     ♔   ♟       3\n\
                      \4 ♛ ♕   ♕ ♖ ♟     4\n\
                      \5 ♜     ♖ ♜ ♝ ♜ ♜ 5\n\
                      \6 ♘   ♛   ♗ ♘   ♞ 6\n\
                      \7           ♘     7\n\
                      \  0 1 2 3 4 5 6 7\n\
                      \  U       U     U"
        expMoves = [Castle Black QueenSide]

prop_noCastlingOtherRow :: Bool
                          -- Check row 0 too, using the same function,
                          -- as a sanity check
prop_noCastlingOtherRow = movesFromBoardWithKingAtRow 0 `listEq` allCastlings &&
                          movesFromBoardWithKingAtRow 2 `listEq` noCastlings &&
                          movesFromBoardWithKingAtRow 7 `listEq` noCastlings
    where
        board = read  "  U       U     U  \n\
                      \  0 1 2 3 4 5 6 7  \n\
                      \0 ♜             ♜ 0\n\
                      \1 ♜             ♜ 1\n\
                      \2 ♜             ♜ 2\n\
                      \3 ♜             ♜ 3\n\
                      \4 ♜             ♜ 4\n\
                      \5 ♜         ♔   ♜ 5\n\
                      \6 ♜             ♜ 6\n\
                      \7 ♜             ♜ 7\n\
                      \  0 1 2 3 4 5 6 7\n\
                      \  U       U     U"
        movesFromBoardWithKingAtRow row = movesFunOnlyCastling Black board'
            where
                board' = setB (Pos row 4) (Piece Black King) $
                         setB (Pos row 4) Empty $ board
                
        noCastlings = []
        allCastlings = [Castle Black KingSide, Castle Black QueenSide]

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

type VerifyMovesFun = [Move] -> Color -> Board -> Property

verifyMovesCustomFun :: MovesFun -> VerifyMovesFun
verifyMovesCustomFun movesFun expMoves' color board =
    counterexample errorString verificationResult
    where
        expMoves = sort expMoves'
        actMoves = sort $ movesFun color board
        verificationResult = expMoves == actMoves
        actualMissing = expMoves \\ actMoves
        actualExtra = actMoves \\ expMoves
        errorString = show board ++ "\n" ++
                      "Expected moves: " ++ show expMoves ++ "\n\n" ++
                      "Actual moves: " ++ show actMoves ++ "\n\n" ++
                      "Actual is missing: " ++ show actualMissing ++ "\n\n" ++
                      "Actual has these extra: " ++ show actualExtra

verifyMoves :: VerifyMovesFun
verifyMoves = verifyMovesCustomFun movesFun

verifyCastlingMoves :: VerifyMovesFun
verifyCastlingMoves = verifyMovesCustomFun movesFunOnlyCastling

movesFunOnlyCastling :: MovesFun
movesFunOnlyCastling color board = filter isCastle $ movesFun color board

normalMovesFrom :: Pos -> [Pos] -> [Move]
normalMovesFrom src dsts = map (NormalMove src) dsts

promotesFrom :: Pos -> [Pos] -> [Move]
promotesFrom src dsts = [Promote src dst k | k <- [Rook, Bishop, Knight, Queen], dst <- dsts]


return []
runTests = $quickCheckAll

