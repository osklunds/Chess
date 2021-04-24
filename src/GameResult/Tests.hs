
{-# LANGUAGE TemplateHaskell #-}

module GameResult.Tests where

import Test.QuickCheck

import GameResult
import Board

prop_resultNormal :: Bool
prop_resultNormal = gameResult Black board == Normal
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

prop_resultCheck :: Bool
prop_resultCheck = gameResult Black board == Check
  where
    board = read  "  0 1 2 3 4 5 6 7  \n\
                  \0 ♟             ♝ 0\n\
                  \1   ♞           ♝ 1\n\
                  \2   ♔     ♝       2\n\
                  \3     ♜   ♙   ♗ ♙ 3\n\
                  \4     ♛   ♖ ♟   ♚ 4\n\
                  \5     ♖         ♜ 5\n\
                  \6       ♘ ♞ ♟ ♝ ♟ 6\n\
                  \7       ♜   ♞ ♖ ♝ 7\n\
                  \  0 1 2 3 4 5 6 7"

prop_resultCheckmate :: Bool
prop_resultCheckmate = gameResult Black board == Checkmate
  where
    board = read  "  0 1 2 3 4 5 6 7  \n\
                  \0                 0\n\
                  \1                 1\n\
                  \2   ♔             2\n\
                  \3                 3\n\
                  \4                 4\n\
                  \5             ♖ ♖ 5\n\
                  \6                 6\n\
                  \7               ♚ 7\n\
                  \  0 1 2 3 4 5 6 7"

prop_resultDraw :: Bool
prop_resultDraw = gameResult Black board == Draw
  where
    board = read  "  0 1 2 3 4 5 6 7  \n\
                  \0                 0\n\
                  \1                 1\n\
                  \2   ♔             2\n\
                  \3                 3\n\
                  \4                 4\n\
                  \5             ♖   5\n\
                  \6             ♖   6\n\
                  \7               ♚ 7\n\
                  \  0 1 2 3 4 5 6 7"



return []
runTests = $quickCheckAll