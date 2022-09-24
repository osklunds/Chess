
{-# LANGUAGE TemplateHaskell #-}

module Moves.CheckUnaware.Tests where

import Test.QuickCheck

import Lib
import qualified Moves.TestLib as MTL
import Board
import qualified Moves.CheckUnaware as CU
import qualified Moves.NormalMoves as NM

--------------------------------------------------------------------------------
-- Fixed boards
--------------------------------------------------------------------------------

prop_fixedBoard1 :: Property
prop_fixedBoard1 = verifyMoves expMoves Black board
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

prop_fixedBoard2 :: Property
prop_fixedBoard2 = verifyMoves expMoves White board
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

verifyMoves :: [Move] -> Color -> Board -> Property
verifyMoves expMoves color board =
    MTL.verifyMoves allExpMoves color board CU.movesForColor
    where
        expNormalMoves = normalMovesForColor color board
        allExpMoves = expMoves ++ expNormalMoves

--------------------------------------------------------------------------------
-- Arbitrary boards
--------------------------------------------------------------------------------

prop_movesIsSupersetOfNormalMoves :: Color -> Board -> Bool
prop_movesIsSupersetOfNormalMoves color board =
    normalMoves `isSubsetOf` moves
    where
        moves        = CU.movesForColor color board
        normalMoves  = normalMovesForColor color board


normalMovesForColor :: Color -> Board -> [Move]
normalMovesForColor c b = map toNormalMove $ NM.movesForColor c b
    where
        toNormalMove ((rowS,colS),(rowD,colD)) =
            (NormalMove (Pos rowS colS) (Pos rowD colD))


return []
runTests = $quickCheckAll
