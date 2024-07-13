
{-# LANGUAGE TemplateHaskell #-}

module Score.Tests where

import Test.QuickCheck hiding (Result)

import Types
import Score


prop_scoreNormal :: Property
prop_scoreNormal = counterexample (show (expScore, scoreValueWhite)) result
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

    expScore = sum [900, -330,
                    -320, -330,
                    0, -330,
                    -500, 100, 100,
                    -900, 500, -100, 330, 0,
                    500, -500,
                    320, -320, -100, -330, -100,
                    -500, -320, 500, -330]
               +
               sum [-10, 20,
                    20, 10,
                    -40, -10,
                    0, 25, 5,
                    -5, 0, -10, 0, 30,
                    0, 5,
                    5, 0, -50, 0, -50,
                    0, 30, 0, 20]

    result = scoreValueBlack == expScore &&
             resultBlack == Normal False &&
             scoreValueWhite == expScore &&
             resultWhite == Normal False

prop_scoreCheck :: Property
prop_scoreCheck = counterexample (show (scoreValueBlack, expScore)) result
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

    expScore = sum [900, -330,
                    -320, -330,
                    0, -500, -330,
                    100, 100,
                    -900, 500, -100, 330, 0,
                    500, -500,
                    320, -320, -100, -330, -100,
                    -500, -320, 500, -330]
               +
               sum [-10, 20,
                    20, 10,
                    -40, 0, -10,
                    25, 5,
                    -5, 0, -10, 0, 30,
                    0, 5,
                    5, 0, -50, 0, -50,
                    0, 30, 0, 20]
    result = scoreValueBlack == expScore &&
             resultBlack == Normal False &&
             scoreValueWhite == expScore &&
             resultWhite == Normal True

prop_boardWhereWhiteIsCheckmated :: Bool
prop_boardWhereWhiteIsCheckmated = scoreValueWhite == minBound &&
                                   resultWhite == Checkmate &&
                                   scoreValueBlack < 0 &&
                                   resultBlack == Normal False
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
prop_boardWhereBlackIsCheckmated = scoreValueBlack == maxBound &&
                                   resultBlack == Checkmate &&
                                   scoreValueWhite < 0 &&
                                   resultWhite == Normal False
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
                               scoreValueBlack < 0 &&
                               resultBlack == Normal False
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
        (scoreValueBlack, resultBlack) = score $ setTurn Black board
        (scoreValueWhite, resultWhite) = score $ setTurn White board


return []
runTests = $quickCheckAll
