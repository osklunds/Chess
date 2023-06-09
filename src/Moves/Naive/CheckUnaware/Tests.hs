
{-# LANGUAGE TemplateHaskell #-}

module Moves.Naive.CheckUnaware.Tests where

import Test.QuickCheck

import Lib
import qualified Moves.Naive.TestLib as MTL
import Types
import Moves.Naive.CheckUnaware
import qualified Moves.Naive.NormalMoves as NM
import TestLib

-- TODO: Port some of these to Check Aware
--------------------------------------------------------------------------------
-- Promotes
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
-- Castling
--------------------------------------------------------------------------------

prop_castlingBoth :: Property
prop_castlingBoth = verifyMoves expMoves White board
    where
        board = read  "  0 1 2 3 4 5 6 7  \n\
                      \0         ♜     ♗ 0\n\
                      \1 ♛ ♕ ♟   ♛       1\n\
                      \2     ♕ ♘ ♔       2\n\
                      \3   ♘   ♙       ♚ 3\n\
                      \4 ♞         ♙   ♟ 4\n\
                      \5         ♟ ♘ ♙   5\n\
                      \6   ♕   ♖   ♟     6\n\
                      \7 ♖       ♔     ♖ 7\n\
                      \  0 1 2 3 4 5 6 7"
        expMoves = [Castle White QueenSide, Castle White KingSide]

prop_castlingKingSide :: Property
prop_castlingKingSide = verifyMoves expMoves Black board
    where
        board = read  "  0 1 2 3 4 5 6 7  \n\
                      \0 ♜   ♘   ♚     ♜ 0\n\
                      \1   ♞ ♛ ♔ ♗ ♕ ♝ ♕ 1\n\
                      \2         ♕ ♛ ♖   2\n\
                      \3   ♜ ♛ ♝     ♞ ♕ 3\n\
                      \4 ♟   ♕ ♞   ♜ ♟ ♟ 4\n\
                      \5       ♕     ♞   5\n\
                      \6   ♞   ♝     ♜ ♘ 6\n\
                      \7                 7\n\
                      \  0 1 2 3 4 5 6 7"
        expMoves = [Castle Black KingSide]

prop_castlingQueenSide :: Property
prop_castlingQueenSide = verifyMoves expMoves Black board
    where
        board = read  "  0 1 2 3 4 5 6 7  \n\
                      \0 ♜       ♚       0\n\
                      \1         ♜ ♝     1\n\
                      \2 ♖ ♕     ♞ ♕     2\n\
                      \3     ♔   ♟       3\n\
                      \4 ♛ ♕   ♕ ♖ ♟     4\n\
                      \5 ♜     ♖ ♜ ♝ ♜ ♜ 5\n\
                      \6 ♘   ♛   ♗ ♘   ♞ 6\n\
                      \7           ♘   ♚ 7\n\
                      \  0 1 2 3 4 5 6 7"
        expMoves = [Castle Black QueenSide]

prop_noCastlingOtherRow :: Board -> Int -> Color -> Bool
prop_noCastlingOtherRow board row color
    | row' == homeRow color = all (`elem` moves) castlings -- Sanity check
    | row' /= homeRow color = not $ any (`elem` moves) castlings
    where
        row' = row `mod` 7

        board' = setB (Pos row' 0) (Piece color Rook) $
                 setB (Pos row' 7) (Piece color Rook) $
                 setB (Pos row' 4) (Piece color King) $
                 removeKing color $ 
                 setEmpty [Pos row' col | col <- [0..7]] board

        moves = movesF color board'
        castlings = [Castle color KingSide, Castle color QueenSide]


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------


return []
runTests = $quickCheckAll
