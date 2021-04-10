
{-# LANGUAGE TemplateHaskell #-}

module MoveSelection.Score.Tests where

import Test.QuickCheck

import Board as B
import Score


prop_fixed1 :: Bool
prop_fixed1 = scoreForColor Black board == score
  where
    board = read  "  0 1 2 3 4 5 6 7  \n\
                  \0 ♟ ♕           ♝ 0\n\
                  \1   ♞           ♝ 1\n\
                  \2   ♔     ♝       2\n\
                  \3     ♜   ♙     ♙ 3\n\
                  \4     ♛   ♖ ♟ ♗ ♚ 4\n\
                  \5     ♖       ♗ ♜ 5\n\
                  \6       ♘ ♞ ♟ ♝ ♟ 6\n\
                  \7       ♜   ♞ ♖ ♝ 7\n\
                  \  0 1 2 3 4 5 6 7"

    score =          1-10 +0+0+0+0+3
                    +0+3+0+0+0+0+0+3
                    +0-0+0+0+3+0+0+0
                    +0+0+5+0-1+0+0-1
                    +0+0+10 -5+1-3+0
                    +0+0-5+0+0+0-3+5
                    +0+0+0-3+3+1+3+1
                    +0+0+0+5+0+3-5+3

prop_noKing :: Board -> Property
prop_noKing board = hasOtherKing ==>
                    scoreForColor Black boardNoOwnKing == minBound
  where
    hasOtherKing   = B.any (== Piece White King) board
    boardNoOwnKing = B.map removeKing board

    removeKing (Piece Black King) = Empty
    removeKing square             = square

prop_noOtherKing :: Board -> Property
prop_noOtherKing board = hasOwnKing ==>
                         scoreForColor Black boardNoOtherKing == maxBound
  where
    hasOwnKing       = B.any (== Piece Black King) board
    boardNoOtherKing = B.map removeKing board

    removeKing (Piece White King) = Empty
    removeKing square             = square





return []
runTests = $quickCheckAll