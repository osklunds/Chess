
{-# LANGUAGE TemplateHaskell #-}

module MoveSelection.Score.Tests where

import Test.QuickCheck

import Board
import MoveSelection.Score


prop_scoreNormal :: Bool
prop_scoreNormal = scoreForBoard board == score
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

prop_scoreCheck :: Bool
prop_scoreCheck = scoreForBoard board == score
  where
    board = read  "  0 1 2 3 4 5 6 7  \n\
                  \0 ♟ ♕           ♝ 0\n\
                  \1   ♞           ♝ 1\n\
                  \2   ♔[♜]  ♝       2\n\
                  \3         ♙     ♙ 3\n\
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

prop_scoreCheckmate :: Bool
prop_scoreCheckmate = scoreForBoard board == maxBound
  where
    board = read  "  0 1 2 3 4 5 6 7  \n\
                  \0 ♟ ♕           ♝ 0\n\
                  \1   ♞           ♝ 1\n\
                  \2   ♔[♜]  ♝       2\n\
                  \3         ♙     ♙ 3\n\
                  \4[♜]  ♛   ♖ ♟ ♗ ♚ 4\n\
                  \5     ♖       ♗ ♜ 5\n\
                  \6       ♘ ♞ ♟ ♝ ♟ 6\n\
                  \7       ♜   ♞ ♖ ♝ 7\n\
                  \  0 1 2 3 4 5 6 7"

prop_scoreDraw :: Bool
prop_scoreDraw = scoreForBoard board == 0
  where
    board = read  "  0 1 2 3 4 5 6 7  \n\
                  \0 ♟[♛]          ♝ 0\n\
                  \1   ♞           ♝ 1\n\
                  \2   ♔     ♝       2\n\
                  \3    [♜]          3\n\
                  \4[♜]  ♛     ♟   ♚ 4\n\
                  \5               ♜ 5\n\
                  \6         ♞ ♟ ♝ ♟ 6\n\
                  \7       ♜   ♞   ♝ 7\n\
                  \  0 1 2 3 4 5 6 7"

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

scoreForBoard :: Board -> Int
scoreForBoard = scoreForColor Black turn
  where
    turn = White


return []
runTests = $quickCheckAll
