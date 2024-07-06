
{-# LANGUAGE TemplateHaskell #-}

module Score.Tests where

import Test.QuickCheck

import Types
import Score


prop_scoreNormal :: Bool
prop_scoreNormal = score board == expScore &&
                   score (invertTurn board) == expScore
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

prop_boardWhereWhiteIsCheckmated :: Bool
prop_boardWhereWhiteIsCheckmated = scoreWhiteTurn == maxBound &&
                                   scoreBlackTurn > 0 &&
                                   scoreBlackTurn < maxBound
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

    scoreWhiteTurn = score board
    scoreBlackTurn = score (invertTurn board)

prop_boardWhereBlackIsCheckmated :: Bool
prop_boardWhereBlackIsCheckmated = scoreBlackTurn == minBound &&
                                   scoreWhiteTurn > 0 &&
                                   scoreWhiteTurn < maxBound
  where
    board = read  "  U       M     M  \n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \0             ♖   0\n\
                  \1         ♙     ♖ 1\n\
                  \2   ♔     ♙       2\n\
                  \3 ♙ ♙ ♙ ♙ ♙       3\n\
                  \4 ♛ ♛ ♛   ♙       4\n\
                  \5 ♛ ♛ ♛   ♙       5\n\
                  \6 ♛ ♛ ♛   ♙     ♚ 6\n\
                  \7                 7\n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \  U       U     U\n\
                  \[White]"

    scoreWhiteTurn = score board
    scoreBlackTurn = score (invertTurn board)

prop_boardWhereWhiteCantMove :: Bool
prop_boardWhereWhiteCantMove = scoreWhiteTurn == 0 &&
                               scoreBlackTurn > 0 &&
                               scoreBlackTurn < maxBound
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

    scoreWhiteTurn = score board
    scoreBlackTurn = score (invertTurn board)

-- TODO: These tests are strange, for a situation that should not happen
prop_scoreWhiteKing :: Bool
prop_scoreWhiteKing = score board == maxBound
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

prop_scoreBlackKing :: Bool
prop_scoreBlackKing = score board == minBound
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
