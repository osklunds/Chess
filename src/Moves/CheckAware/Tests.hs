
{-# LANGUAGE TemplateHaskell #-}

module Moves.CheckAware.Tests where

import Data.List
import Test.QuickCheck

import Board
import Moves.CheckAware
import qualified Moves.CheckUnaware as CU


--------------------------------------------------------------------------------
-- Fixed boards
--------------------------------------------------------------------------------

prop_fixedBoard1 :: Property
prop_fixedBoard1 = verifyMoves Black board moves
  where
    board = read  "  0 1 2 3 4 5 6 7  \n\
                  \0 ♟             ♝ 0\n\
                  \1   ♞           ♝ 1\n\
                  \2   ♔     ♝       2\n\
                  \3     ♜   ♙     ♙ 3\n\
                  \4     ♛   ♖ ♟ ♗ ♚ 4\n\
                  \5     ♖         ♜ 5\n\
                  \6       ♘ ♞ ♟ ♝ ♟ 6\n\
                  \7       ♜   ♞ ♖ ♝ 7\n\
                  \  0 1 2 3 4 5 6 7"
    moves = -- King at (4,7)
            (movesFrom (4,7) [(3,6), (4,6)]) ++
            -- Note that (3,7) and (5,6) are missing as destinations

            -- Queen at (4,2)
            (movesFrom (4,2) [(4,3), (4,4), (5,2), (5,1), (6,0), (4,1), (4,0),
                              (3,1), (2,0), (3,3), (5,3)]) ++

            -- Bishop at (0,7)
            (movesFrom (0,7) [(1,6), (2,5), (3,4)]) ++

            -- Bishop at (1,7)
            (movesFrom (1,7) [(2,6), (3,5), (4,4), (0,6)]) ++

            -- Bishop at (2,4)
            (movesFrom (2,4) [(1,5), (0,6), (3,5), (4,6), (3,3), (1,3),
                              (0,2)]) ++

            -- Bishop at (6,6)
            (movesFrom (6,6) [(5,5), (4,4)]) ++

            -- Pawn at (0,0)
            (movesFrom (0,0) [(1,0)]) ++

            -- Pawn at (6,5)
            (movesFrom (6,5) [(7,6)]) ++

            -- Pawn at (6,7)
            (movesFrom (6,7) [(7,6)]) ++

            -- Pawn at (4,5)
            (movesFrom (4,5) [(5,5)]) ++

            -- Rook at (7,3)
            (movesFrom (7,3) [(7,4), (7,2), (7,1), (6,3), (7,0)]) ++

            -- Rook at (5,7)
            (movesFrom (5,7) [(5,6), (5,5), (5,4), (5,3), (5,2)]) ++

            -- Rook at (3,2)
            (movesFrom (3,2) [(2,2), (1,2), (0,2), (3,3), (3,4), (3,1),
                              (3,0)]) ++

            -- Knight at (1,1)
            (movesFrom (1,1) [(0,3), (2,3), (3,0)]) ++

            -- Knight at (7,5)
            (movesFrom (7,5) [(5,6), (5,4), (6,3)]) ++

            -- Knight at (6,4)
            (movesFrom (6,4) [(4,3), (5,6), (7,6), (7,2), (5,2)])

prop_fixedBoardAllKindsPreventMove :: Property
prop_fixedBoardAllKindsPreventMove = verifyMoves White board moves
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
    moves = movesFrom (5,3) [(6,4)]

prop_fixedBoardMovesAwayIfIsChecked :: Property
prop_fixedBoardMovesAwayIfIsChecked = verifyMoves White board moves
  where
    board = read  "  0 1 2 3 4 5 6 7  \n\
                  \0                 0\n\
                  \1                 1\n\
                  \2                 2\n\
                  \3                 3\n\
                  \4                 4\n\
                  \5 ♜     ♔         5\n\
                  \6                 6\n\
                  \7               ♚ 7\n\
                  \  0 1 2 3 4 5 6 7"
    moves = movesFrom (5,3) [(4,2), (4,3), (4,4), (6,2), (6,3), (6,4)]

prop_fixedBoardBlocksWithOther :: Property
prop_fixedBoardBlocksWithOther = verifyMoves White board moves
  where
    board = read  "  0 1 2 3 4 5 6 7  \n\
                  \0 ♚               0\n\
                  \1                 1\n\
                  \2                 2\n\
                  \3               ♛ 3\n\
                  \4             ♜ ♝ 4\n\
                  \5 ♜             ♔ 5\n\
                  \6       ♙     ♜ ♝ 6\n\
                  \7               ♛ 7\n\
                  \  0 1 2 3 4 5 6 7"
    moves = movesFrom (6,3) [(5,3)]

prop_fixedBoardCapturesThreat :: Property
prop_fixedBoardCapturesThreat = verifyMoves White board moves
  where
    board = read  "  0 1 2 3 4 5 6 7  \n\
                  \0 ♚         ♗     0\n\
                  \1                 1\n\
                  \2                 2\n\
                  \3               ♛ 3\n\
                  \4             ♜ ♝ 4\n\
                  \5 ♜             ♔ 5\n\
                  \6             ♜ ♝ 6\n\
                  \7               ♛ 7\n\
                  \  0 1 2 3 4 5 6 7"
    moves = movesFrom (0,5) [(5,0)]



movesFrom :: (Int,Int) -> [(Int,Int)] -> [((Int,Int),(Int,Int))]
movesFrom start ends = map (\end -> (start, end)) ends


--------------------------------------------------------------------------------
-- Arbitrary boards
--------------------------------------------------------------------------------

prop_movesIsSubsetOfCheckUnawareMoves :: Color -> Board -> Bool
prop_movesIsSubsetOfCheckUnawareMoves color board =
  moves `isSubsetOf` movesCheckUnaware
  where
    moves             = movesForColor color board
    movesCheckUnaware = CU.movesForColor color board


return []
runTests = $quickCheckAll
