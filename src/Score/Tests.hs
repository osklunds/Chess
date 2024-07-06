
{-# LANGUAGE TemplateHaskell #-}

module Score.Tests where

import Test.QuickCheck

import Types
import Score


prop_scoreNormal :: Bool
prop_scoreNormal = score board == expScore
  where
    board = read  "  U       M     U  \n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \0   ♕           ♝ 0\n\
                  \1 ♟ ♞           ♝ 1\n\
                  \2   ♔     ♝       2\n\
                  \3     ♜   ♙     ♙ 3\n\
                  \4     ♛   ♖ ♟ ♗ ♚ 4\n\
                  \5     ♖       ♗ ♜ 5\n\
                  \6       ♘ ♞ ♟ ♝ ♟ 6\n\
                  \7       ♜   ♞ ♖ ♝ 7\n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \  U       M     M\n\
                  \[Black]"

    expScore =       0-10 +0+0+0+0+3
                    +1+3+0+0+0+0+0+3
                    +0-0+0+0+3+0+0+0
                    +0+0+5+0-1+0+0-1
                    +0+0+10 -5+1-3+0
                    +0+0-5+0+0+0-3+5
                    +0+0+0-3+3+1+3+1
                    +0+0+0+5+0+3-5+3

prop_scoreCheck :: Bool
prop_scoreCheck = score board == expScore
  where
    board = read  "  U       U     U  \n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \0   ♕           ♝ 0\n\
                  \1 ♟ ♞           ♝ 1\n\
                  \2   ♔[♜]  ♝       2\n\
                  \3         ♙     ♙ 3\n\
                  \4     ♛   ♖ ♟ ♗ ♚ 4\n\
                  \5     ♖       ♗ ♜ 5\n\
                  \6       ♘ ♞ ♟ ♝ ♟ 6\n\
                  \7       ♜   ♞ ♖ ♝ 7\n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \  U       M     U\n\
                  \[Black]"

    expScore =       0-10 +0+0+0+0+3
                    +1+3+0+0+0+0+0+3
                    +0-0+0+0+3+0+0+0
                    +0+0+5+0-1+0+0-1
                    +0+0+10 -5+1-3+0
                    +0+0-5+0+0+0-3+5
                    +0+0+0-3+3+1+3+1
                    +0+0+0+5+0+3-5+3

prop_scoreCheckmate :: Bool
prop_scoreCheckmate = score board == maxBound
  where
    board = read  "  U       M     M  \n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \0 ♝ ♕           ♝ 0\n\
                  \1   ♞           ♝ 1\n\
                  \2   ♔[♜]  ♝       2\n\
                  \3         ♙     ♙ 3\n\
                  \4[♜]  ♛   ♖ ♟ ♗ ♚ 4\n\
                  \5     ♖       ♗ ♜ 5\n\
                  \6       ♘ ♞ ♟ ♝ ♟ 6\n\
                  \7       ♜   ♞ ♖ ♝ 7\n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \  U       U     U\n\
                  \[White]"

prop_scoreDraw :: Bool
prop_scoreDraw = score board == 0
  where
    board = read  "  U       M     U  \n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \0 ♝[♛]          ♝ 0\n\
                  \1   ♞           ♝ 1\n\
                  \2   ♔     ♝       2\n\
                  \3    [♜]          3\n\
                  \4[♜]  ♛     ♟   ♚ 4\n\
                  \5               ♜ 5\n\
                  \6         ♞ ♟ ♝ ♟ 6\n\
                  \7       ♜   ♞   ♝ 7\n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \  U       M     M\n\
                  \[White]"

prop_scoreNoOtherKing :: Bool
prop_scoreNoOtherKing = score board == maxBound
  where
    board = read  "  U       M     U  \n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \0           ♕ ♝   0\n\
                  \1   ♝         ♜   1\n\
                  \2 ♛   ♛   ♟ ♙ ♛   2\n\
                  \3 ♜         ♕     3\n\
                  \4   ♘         ♗ ♘ 4\n\
                  \5 ♜           ♟ ♘ 5\n\
                  \6 ♗ ♚ ♜   ♜       6\n\
                  \7     ♗ ♖         7\n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \  U       M     M\n\
                  \[Black]"

prop_scoreNoOwnKing :: Bool
prop_scoreNoOwnKing = score board == minBound
  where
    board = read  "  U       M     U  \n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \0         ♛   ♔   0\n\
                  \1 ♖       ♜ ♟ ♘   1\n\
                  \2     ♗           2\n\
                  \3 ♛   ♝ ♞       ♝ 3\n\
                  \4 ♝ ♗ ♝           4\n\
                  \5       ♞   ♝     5\n\
                  \6 ♞ ♘ ♜         ♛ 6\n\
                  \7   ♜   ♘         7\n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \  U       M     M\n\
                  \[Black]"

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------


return []
runTests = $quickCheckAll
