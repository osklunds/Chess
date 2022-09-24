
{-# LANGUAGE TemplateHaskell #-}

module Moves.Naive.CheckAware.Tests where

import Data.List
import Test.QuickCheck

import Board
import qualified Moves.Naive.CheckAware as CA
import qualified Moves.Naive.CheckUnaware as CU
import qualified Moves.Naive.TestLib as MTL
import Lib


--------------------------------------------------------------------------------
-- Fixed boards
--------------------------------------------------------------------------------

prop_fixedBoard1 :: Property
prop_fixedBoard1 = verifyMoves expMoves Black board
  where
    board = read  "  0 1 2 3 4 5 6 7  \n\
                  \0 ♟             ♝ 0\n\
                  \1   ♞           ♝ 1\n\
                  \2   ♔     ♝       2\n\
                  \3     ♜   ♙     ♙ 3\n\
                  \4     ♛   ♖ ♟ ♗ ♚ 4\n\
                  \5     ♖         ♜ 5\n\
                  \6       ♘ ♞ ♟ ♝ ♟ 6\n\
                  \7   ♟   ♜   ♞ ♖ ♝ 7\n\
                  \  0 1 2 3 4 5 6 7"
    expMoves = -- King at (4 7)
               (normalMovesFrom (Pos 4 7) [(Pos 3 6), (Pos 4 6)]) ++
               -- Note that (Pos 3 7) and (Pos 5 6) are missing as destinations

               -- Queen at (4 2)
               (normalMovesFrom (Pos 4 2) [(Pos 4 3), (Pos 4 4), (Pos 5 2),
                                           (Pos 5 1), (Pos 6 0), (Pos 4 1),
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

               -- Pawn at (0 0)
               (normalMovesFrom (Pos 0 0) [(Pos 1 0)]) ++

               -- Pawn at (6 5)
               (normalMovesFrom (Pos 6 5) [(Pos 7 6)]) ++

               -- Pawn at (6 7)
               (normalMovesFrom (Pos 6 7) [(Pos 7 6)]) ++

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

               -- Knight at (7 5)
               (normalMovesFrom (Pos 7 5) [(Pos 5 6), (Pos 5 4), (Pos 6 3)]) ++

               -- Knight at (6 4)
               (normalMovesFrom (Pos 6 4) [(Pos 4 3), (Pos 5 6), (Pos 7 6),
                                           (Pos 7 2), (Pos 5 2)
                                          ]) ++

               -- Pawn at (7 1)
               promotesAt (Pos 7 1)

prop_fixedBoardAllKindsPreventMove :: Property
prop_fixedBoardAllKindsPreventMove = verifyMoves moves White board
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

prop_fixedBoardMovesAwayIfIsChecked :: Property
prop_fixedBoardMovesAwayIfIsChecked = verifyMoves moves White board
  where
    board = read  "  0 1 2 3 4 5 6 7  \n\
                  \0       ♙         0\n\
                  \1 ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟ 1\n\
                  \2                 2\n\
                  \3                 3\n\
                  \4                 4\n\
                  \5 ♜     ♔         5\n\
                  \6                 6\n\
                  \7               ♚ 7\n\
                  \  0 1 2 3 4 5 6 7"
    moves = normalMovesFrom (Pos 5 3) [(Pos 4 2), (Pos 4 3), (Pos 4 4),
                                       (Pos 6 2), (Pos 6 3), (Pos 6 4)]

prop_fixedBoardBlocksWithOther :: Property
prop_fixedBoardBlocksWithOther = verifyMoves moves White board
  where
    board = read  "  0 1 2 3 4 5 6 7  \n\
                  \0 ♚     ♘     ♙   0\n\
                  \1 ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟ 1\n\
                  \2                 2\n\
                  \3               ♛ 3\n\
                  \4             ♜ ♝ 4\n\
                  \5 ♜             ♔ 5\n\
                  \6       ♙     ♜ ♝ 6\n\
                  \7               ♛ 7\n\
                  \  0 1 2 3 4 5 6 7"
    moves = normalMovesFrom (Pos 6 3) [(Pos 5 3)]

prop_fixedBoardCapturesThreat :: Property
prop_fixedBoardCapturesThreat = verifyMoves moves White board
  where
    board = read  "  0 1 2 3 4 5 6 7  \n\
                  \0 ♚   ♘   ♙ ♗     0\n\
                  \1                 1\n\
                  \2                 2\n\
                  \3               ♛ 3\n\
                  \4             ♜ ♝ 4\n\
                  \5 ♜             ♔ 5\n\
                  \6             ♜ ♝ 6\n\
                  \7               ♛ 7\n\
                  \  0 1 2 3 4 5 6 7"
    moves = normalMovesFrom (Pos 0 5) [(Pos 5 0)]

verifyMoves :: [Move] -> Color -> Board -> Property
verifyMoves expMoves color board =
    MTL.verifyMoves expMoves color board CA.movesF

normalMovesFrom :: Pos -> [Pos] -> [Move]
normalMovesFrom src dsts = map (NormalMove src) dsts

promotesAt :: Pos -> [Move]
promotesAt p = [Promote p k | k <- [Rook, Bishop, Knight, Queen]]

--------------------------------------------------------------------------------
-- Arbitrary boards
--------------------------------------------------------------------------------

prop_movesIsSubsetOfCheckUnawareMoves :: Color -> Board -> Bool
prop_movesIsSubsetOfCheckUnawareMoves color board =
  moves `isSubsetOf` movesCheckUnaware
  where
    moves             = CA.movesF color board
    movesCheckUnaware = CU.movesF color board


return []
runTests = $quickCheckAll