
{-# LANGUAGE TemplateHaskell #-}

module Score.Tests where

import Test.QuickCheck hiding (Result)

import Types
import Score


prop_scoreNormal :: Bool
prop_scoreNormal = scoreValueBlack == expScore &&
                   resultBlack == Normal &&
                   scoreValueWhite == expScore &&
                   resultWhite == Normal
  where
    board = read  "  U       M     U  \n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \0   ♕           ♝ 0\n\
                  \1   ♞           ♝ 1\n\
                  \2   ♔     ♝       2\n\
                  \3     ♜   ♙     ♙ 3\n\
                  \4     ♛   ♖ ♟ ♗ ♚ 4\n\
                  \5     ♖         ♜ 5\n\
                  \6       ♘ ♞ ♟ ♝ ♟ 6\n\
                  \7       ♜   ♞ ♖ ♝ 7\n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \  U       M     M\n\
                  \[Black]"

    (scoreValueBlack, resultBlack, scoreValueWhite, resultWhite) =
        getValues board

    expScore =       0-10 +0+0+0+0+3
                    +0+3+0+0+0+0+0+3
                    +0-0+0+0+3+0+0+0
                    +0+0+5+0-1+0+0-1
                    +0+0+10 -5+1-3+0
                    +0+0-5+0+0+0-0+5
                    +0+0+0-3+3+1+3+1
                    +0+0+0+5+0+3-5+3

prop_scoreCheck :: Bool
prop_scoreCheck = scoreValueBlack == expScore &&
                  resultBlack == Normal &&
                  scoreValueWhite == expScore &&
                  resultWhite == Check
  where
    board = read  "  U       U     U  \n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \0   ♕           ♝ 0\n\
                  \1   ♞           ♝ 1\n\
                  \2   ♔[♜]  ♝       2\n\
                  \3         ♙     ♙ 3\n\
                  \4     ♛   ♖ ♟ ♗ ♚ 4\n\
                  \5     ♖         ♜ 5\n\
                  \6       ♘ ♞ ♟ ♝ ♟ 6\n\
                  \7       ♜   ♞ ♖ ♝ 7\n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \  U       M     U\n\
                  \[Black]"

    (scoreValueBlack, resultBlack, scoreValueWhite, resultWhite) =
        getValues board

    expScore =       0-10 +0+0+0+0+3
                    +0+3+0+0+0+0+0+3
                    +0-0+0+0+3+0+0+0
                    +0+0+5+0-1+0+0-1
                    +0+0+10 -5+1-3+0
                    +0+0-5+0+0+0-0+5
                    +0+0+0-3+3+1+3+1
                    +0+0+0+5+0+3-5+3

prop_boardWhereWhiteIsCheckmated :: Bool
prop_boardWhereWhiteIsCheckmated = scoreValueWhite == maxBound &&
                                   resultWhite == Checkmate &&
                                   scoreValueBlack > 0 &&
                                   scoreValueBlack < maxBound &&
                                   resultBlack == Normal
  where
    board = read  "  U       M     M  \n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \0 ♝ ♕           ♝ 0\n\
                  \1   ♞           ♝ 1\n\
                  \2   ♔[♜]  ♝       2\n\
                  \3         ♙     ♙ 3\n\
                  \4[♜]  ♛   ♖ ♟ ♗ ♚ 4\n\
                  \5     ♖         ♜ 5\n\
                  \6       ♘ ♞ ♟ ♝ ♟ 6\n\
                  \7       ♜   ♞ ♖ ♝ 7\n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \  U       U     U\n\
                  \[Black]"

    (scoreValueBlack, resultBlack, scoreValueWhite, resultWhite) =
        getValues board

prop_boardWhereBlackIsCheckmated :: Bool
prop_boardWhereBlackIsCheckmated = scoreValueBlack == minBound &&
                                   resultBlack == Checkmate &&
                                   scoreValueWhite > 0 &&
                                   scoreValueWhite < maxBound &&
                                   resultWhite == Normal
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
                  \[Black]"

    (scoreValueBlack, resultBlack, scoreValueWhite, resultWhite) =
        getValues board

prop_boardWhereWhiteCantMove :: Bool
prop_boardWhereWhiteCantMove = scoreValueWhite == 0 &&
                               resultWhite == Draw &&
                               scoreValueBlack > 0 &&
                               scoreValueBlack < maxBound &&
                               resultBlack == Normal
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
                  \[Black]"

    (scoreValueBlack, resultBlack, scoreValueWhite, resultWhite) =
        getValues board

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

getValues :: Board -> (Int, Result, Int, Result)
getValues board = (scoreValueBlack, resultBlack, scoreValueWhite, resultWhite)
    where
        (scoreValueBlack, resultBlack, _movesBlack) = score $ setTurn Black board
        (scoreValueWhite, resultWhite, _movesWhite) = score $ setTurn White board


return []
runTests = $quickCheckAll
