
{-# LANGUAGE TemplateHaskell #-}

module Moves.Naive.CheckAware.Tests where

import Data.List
import Test.QuickCheck

import Types
import Moves.Naive.CheckAware
import qualified Moves.Naive.CheckUnaware as CU
import qualified Moves.Naive.TestLib as MTL
import Moves.Common
import Lib

-- New strategy: Put most tests here. Use lots of hard coded boards, also for
-- "simple" sitations". Delete all normal move tests. Only add check unaware
-- tests if it's needed for debugging.


--------------------------------------------------------------------------------
-- General
--------------------------------------------------------------------------------

prop_board1 :: Property
prop_board1 = verifyMoves expMoves Black board
  where
    board = read  "  0 1 2 3 4 5 6 7  \n\
                  \0               ♝ 0\n\
                  \1 ♟ ♞           ♝ 1\n\
                  \2   ♔     ♝       2\n\
                  \3     ♜   ♙     ♙ 3\n\
                  \4     ♛   ♖ ♟ ♗ ♚ 4\n\
                  \5     ♖         ♜ 5\n\
                  \6 ♟     ♘ ♞ ♟ ♝ ♟ 6\n\
                  \7   ♞   ♜   ♞ ♖ ♝ 7\n\
                  \  0 1 2 3 4 5 6 7"
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
        blackMoves         = movesF Black board
        swappedColors      = swapColors board
        mirroredBoard      = mirrorBoard swappedColors
        whiteMoves         = movesF White mirroredBoard
        mirroredWhiteMoves = map mirrorMove whiteMoves

swapColors :: Board -> Board
swapColors = mapB swapColor

swapColor :: Square -> Square
swapColor Empty = Empty
swapColor (Piece White kind) = Piece Black kind
swapColor (Piece Black kind) = Piece White kind

mirrorBoard :: Board -> Board
mirrorBoard board = foldl mirrorBoardAtPos
                          board
                          [Pos row col | row <- [0..3], col <- [0..7]]

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


--------------------------------------------------------------------------------
-- Kind specific
--
-- These tests focus on boards with interesting scenarios for the different
-- kinds of pieces.  However, moves for all pieces are still verified to
-- increase the total coverage and to test scenarios that are
-- randomly/accidentally created However, moves for all pieces are still
-- verified to increase the total coverage and to test scenarios that are
-- randomly/accidentally created.
--------------------------------------------------------------------------------

prop_pawns :: Property
prop_pawns = verifyMoves expMoves Black board
  where
    board = read  "  0 1 2 3 4 5 6 7  \n\
                  \0 ♚             ♔ 0\n\
                  \1   ♟     ♟   ♗ ♟ 1\n\
                  \2       ♖   ♗     2\n\
                  \3 ♟ ♕ ♖           3\n\
                  \4   ♙ ♝     ♗     4\n\
                  \5 ♜       ♛   ♘ ♞ 5\n\
                  \6   ♗ ♟ ♟     ♟   6\n\
                  \7     ♜     ♘   ♖ 7\n\
                  \  0 1 2 3 4 5 6 7"
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


--------------------------------------------------------------------------------
-- Check and king
--------------------------------------------------------------------------------

prop_allKindsPreventKingMove :: Property
prop_allKindsPreventKingMove = verifyMoves moves White board
  where
    board = read  "  0 1 2 3 4 5 6 7  \n\
                  \0                 0\n\
                  \1   ♛       ♝     1\n\
                  \2                 2\n\
                  \3   ♞             3\n\
                  \4   ♟       ♚     4\n\
                  \5       ♔         5\n\
                  \6 ♜     ♙         6\n\
                  \7                 7\n\
                  \  0 1 2 3 4 5 6 7"
    moves = normalMovesFrom (Pos 5 3) [(Pos 6 4)]

prop_kingMovesAwayIfChecked :: Property
prop_kingMovesAwayIfChecked = verifyMoves moves White board
  where
    board = read  "  0 1 2 3 4 5 6 7  \n\
                  \0                 0\n\
                  \1 ♟ ♟ ♟ ♙ ♟ ♟ ♟ ♟ 1\n\
                  \2                 2\n\
                  \3                 3\n\
                  \4                 4\n\
                  \5 ♜     ♔         5\n\
                  \6                 6\n\
                  \7               ♚ 7\n\
                  \  0 1 2 3 4 5 6 7"
    moves = normalMovesFrom (Pos 5 3) [(Pos 4 2), (Pos 4 3), (Pos 4 4),
                                       (Pos 6 2), (Pos 6 3), (Pos 6 4)]

prop_blocksWithOtherIfKingIsChecked :: Property
prop_blocksWithOtherIfKingIsChecked = verifyMoves moves White board
  where
    board = read  "  0 1 2 3 4 5 6 7  \n\
                  \0 ♚     ♘         0\n\
                  \1 ♟ ♟ ♟ ♟ ♟ ♟ ♙ ♟ 1\n\
                  \2                 2\n\
                  \3               ♛ 3\n\
                  \4     ♖       ♜ ♝ 4\n\
                  \5 ♜             ♔ 5\n\
                  \6       ♙     ♜ ♝ 6\n\
                  \7               ♛ 7\n\
                  \  0 1 2 3 4 5 6 7"
    moves = (normalMovesFrom (Pos 6 3) [(Pos 5 3)]) ++
            (normalMovesFrom (Pos 4 2) [(Pos 5 2)])

prop_capturesThreatIfKingIsChecked :: Property
prop_capturesThreatIfKingIsChecked = verifyMoves moves White board
  where
    board = read  "  0 1 2 3 4 5 6 7  \n\
                  \0 ♚   ♘     ♗     0\n\
                  \1               ♙ 1\n\
                  \2               ♛ 2\n\
                  \3               ♛ 3\n\
                  \4             ♜ ♝ 4\n\
                  \5 ♜             ♔ 5\n\
                  \6             ♜ ♝ 6\n\
                  \7               ♛ 7\n\
                  \  0 1 2 3 4 5 6 7"
    moves = normalMovesFrom (Pos 0 5) [(Pos 5 0)]

prop_kingCanCaptureThreat :: Property
prop_kingCanCaptureThreat = verifyMoves moves White board
  where
    board = read  "  0 1 2 3 4 5 6 7  \n\
                  \0 ♚   ♘     ♗     0\n\
                  \1               ♙ 1\n\
                  \2           ♝     2\n\
                  \3                 3\n\
                  \4                 4\n\
                  \5             ♜ ♔ 5\n\
                  \6                 6\n\
                  \7             ♝   7\n\
                  \  0 1 2 3 4 5 6 7"
    moves = normalMovesFrom (Pos 5 7) [(Pos 5 6)]

prop_checkedMultipleTypesOfMoves :: Property
prop_checkedMultipleTypesOfMoves = verifyMoves moves White board
  where
    board = read  "  0 1 2 3 4 5 6 7  \n\
                  \0 ♚   ♘     ♗     0\n\
                  \1               ♙ 1\n\
                  \2                 2\n\
                  \3                 3\n\
                  \4                 4\n\
                  \5             ♜ ♔ 5\n\
                  \6           ♙     6\n\
                  \7                 7\n\
                  \  0 1 2 3 4 5 6 7"
    moves = (normalMovesFrom (Pos 5 7) [(Pos 5 6), (Pos 4 7), (Pos 6 7)]) ++
            (normalMovesFrom (Pos 6 5) [(Pos 5 6)])

prop_kingPinned :: Property
prop_kingPinned = verifyMoves moves White board
  where
    board = read  "  0 1 2 3 4 5 6 7  \n\
                  \0                 0\n\
                  \1                 1\n\
                  \2       ♚         2\n\
                  \3                 3\n\
                  \4                 4\n\
                  \5   ♜   ♗   ♔     5\n\
                  \6                 6\n\
                  \7                 7\n\
                  \  0 1 2 3 4 5 6 7"
    moves = (normalMovesFrom (Pos 5 5) [(Pos 5 4), (Pos 4 4), (Pos 4 5),
                                        (Pos 4 6), (Pos 5 6), (Pos 6 6),
                                        (Pos 6 5), (Pos 6 4)])

prop_movesAreSubsetOfCheckUnawareMoves :: Color -> Board -> Bool
prop_movesAreSubsetOfCheckUnawareMoves color board =
  moves `isSubsetOf` movesCheckUnaware
  where
    moves             = movesF color board
    movesCheckUnaware = CU.movesF color board

--------------------------------------------------------------------------------
-- Castling
--------------------------------------------------------------------------------

prop_noCastlingSinceKingWouldPassThreat :: Property
prop_noCastlingSinceKingWouldPassThreat = verifyCastlingMoves moves White board
    where
        board = read  "  0 1 2 3 4 5 6 7  \n\
                      \0   ♜ ♚     ♖ ♘   0\n\
                      \1 ♕   ♞ ♘ ♟ ♝ ♘   1\n\
                      \2       ♞ ♛       2\n\
                      \3   ♖ ♖   ♝     ♟ 3\n\
                      \4       ♜     ♞   4\n\
                      \5   ♙       ♜     5\n\
                      \6           ♖     6\n\
                      \7 ♖       ♔     ♖ 7\n\
                      \  0 1 2 3 4 5 6 7"
        moves = [Castle White KingSide]

prop_noCastlingSinceKingIsChecked :: Property
prop_noCastlingSinceKingIsChecked = verifyCastlingMoves moves White board
    where
        board = read  "  0 1 2 3 4 5 6 7  \n\
                      \0 ♜         ♚   ♜ 0\n\
                      \1 ♟   ♘     ♗ ♘   1\n\
                      \2   ♕   ♙ ♗   ♟ ♞ 2\n\
                      \3 ♘ ♗         ♙   3\n\
                      \4         ♞   ♟   4\n\
                      \5   ♗     ♗ ♙     5\n\
                      \6       ♙   ♝ ♘   6\n\
                      \7 ♖       ♔     ♖ 7\n\
                      \  0 1 2 3 4 5 6 7"
        moves = []

prop_noCastlingSinceDstAttacked :: Property
prop_noCastlingSinceDstAttacked = verifyCastlingMoves moves Black board
    where
        board = read  "  0 1 2 3 4 5 6 7  \n\
                      \0 ♜       ♚     ♜ 0\n\
                      \1           ♝     1\n\
                      \2   ♛     ♝   ♖   2\n\
                      \3 ♙           ♝   3\n\
                      \4   ♖ ♛ ♝     ♝ ♔ 4\n\
                      \5 ♙     ♞         5\n\
                      \6   ♟   ♜ ♘ ♕     6\n\
                      \7   ♖ ♜   ♗     ♜ 7\n\
                      \  0 1 2 3 4 5 6 7"
        moves = [Castle Black QueenSide]

prop_bothCastlingsPossible :: Property
prop_bothCastlingsPossible = verifyCastlingMoves moves Black board
    where
        board = read  "  0 1 2 3 4 5 6 7  \n\
                      \0 ♜       ♚     ♜ 0\n\
                      \1     ♟ ♟ ♜ ♝     1\n\
                      \2 ♝   ♕     ♗   ♙ 2\n\
                      \3       ♝         3\n\
                      \4 ♟   ♝ ♝ ♝       4\n\
                      \5 ♘ ♙   ♖ ♙       5\n\
                      \6         ♙ ♔     6\n\
                      \7   ♜   ♞         7\n\
                      \  0 1 2 3 4 5 6 7"
        moves = [Castle Black QueenSide, Castle Black KingSide]


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

verifyMoves :: [Move] -> Color -> Board -> Property
verifyMoves = MTL.verifyMoves movesF

verifyCastlingMoves :: [Move] -> Color -> Board -> Property
verifyCastlingMoves = MTL.verifyMoves movesF'
    where
        movesF' color board = filter isCastle $ movesF color board

normalMovesFrom :: Pos -> [Pos] -> [Move]
normalMovesFrom src dsts = map (NormalMove src) dsts

promotesFrom :: Pos -> [Pos] -> [Move]
promotesFrom src dsts = [Promote src dst k | k <- [Rook, Bishop, Knight, Queen], dst <- dsts]


return []
runTests = $quickCheckAll
