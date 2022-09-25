
{-# LANGUAGE TemplateHaskell #-}

module Moves.Naive.CheckUnaware.Tests where

import Test.QuickCheck

import Lib
import qualified Moves.Naive.TestLib as MTL
import Board
import Moves.Naive.CheckUnaware
import qualified Moves.Naive.NormalMoves as NM

--------------------------------------------------------------------------------
-- Fixed boards for promotes
--------------------------------------------------------------------------------

prop_fixedBoardPromotes1 :: Property
prop_fixedBoardPromotes1 = verifyMoves expMoves Black board
    where
        board = read  "  0 1 2 3 4 5 6 7  \n\
                      \0 ♗   ♘   ♙     ♞ 0\n\
                      \1   ♟ ♞ ♜ ♛ ♕   ♗ 1\n\
                      \2 ♕   ♕ ♟ ♚   ♖   2\n\
                      \3 ♙   ♟   ♕       3\n\
                      \4 ♟   ♜ ♙ ♙     ♞ 4\n\
                      \5     ♖ ♗     ♞ ♝ 5\n\
                      \6 ♝ ♟             6\n\
                      \7 ♗   ♞ ♟ ♔ ♘ ♟ ♙ 7\n\
                      \  0 1 2 3 4 5 6 7"
        expMoves = promotesAt [Pos 7 3, Pos 7 6]

prop_fixedBoardPromotes2 :: Property
prop_fixedBoardPromotes2 = verifyMoves expMoves White board
    where
        board = read  "  0 1 2 3 4 5 6 7  \n\
                      \0 ♙ ♙ ♙ ♛     ♟ ♙ 0\n\
                      \1 ♟ ♖   ♞ ♛   ♛ ♖ 1\n\
                      \2 ♞ ♙             2\n\
                      \3     ♛ ♗ ♗ ♕ ♙   3\n\
                      \4 ♜ ♞ ♘         ♝ 4\n\
                      \5 ♗   ♛   ♟   ♘ ♕ 5\n\
                      \6 ♗         ♞ ♛ ♚ 6\n\
                      \7   ♙     ♔ ♟ ♞   7\n\
                      \  0 1 2 3 4 5 6 7"
        expMoves = promotesAt [Pos 0 0, Pos 0 1, Pos 0 2, Pos 0 7]

promotesAt :: [Pos] -> [Move]
promotesAt ps = [Promote p k | p <- ps, k <- [Rook, Bishop, Knight, Queen]]

--------------------------------------------------------------------------------
-- Arbitrary boards
--------------------------------------------------------------------------------

prop_movesAreSupersetOfNormalMoves :: Color -> Board -> Bool
prop_movesAreSupersetOfNormalMoves color board =
    normalMoves `isSubsetOf` moves
    where
        moves        = movesF color board
        normalMoves  = normalMovesF color board

prop_blackAndWhiteGiveSameMoves :: Board -> Bool
prop_blackAndWhiteGiveSameMoves =
    MTL.prop_blackAndWhiteGiveSameMoves movesF

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

verifyMoves :: [Move] -> Color -> Board -> Property
verifyMoves expMoves color board =
    MTL.verifyMoves movesF allExpMoves color board
    where
        expNormalMoves = normalMovesF color board
        allExpMoves = expMoves ++ expNormalMoves

normalMovesF :: Color -> Board -> [Move]
normalMovesF c b = map toNormalMove $ NM.movesF c b
    where
        toNormalMove ((rowS,colS),(rowD,colD)) =
            (NormalMove (Pos rowS colS) (Pos rowD colD))


return []
runTests = $quickCheckAll
