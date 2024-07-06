
{-# LANGUAGE TemplateHaskell #-}

module Score.Tests where

import Test.QuickCheck

import Types
import Score


prop_scoreNormal :: Bool
prop_scoreNormal = scoreValueBlack == expScore &&
                   isThreatenedBlack == False &&
                   scoreValueWhite == expScore &&
                   isThreatenedWhite == False
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

    (scoreValueBlack, isThreatenedBlack, scoreValueWhite, isThreatenedWhite) =
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
                  isThreatenedBlack == False &&
                  scoreValueWhite == expScore &&
                  isThreatenedWhite == True
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

    (scoreValueBlack, isThreatenedBlack, scoreValueWhite, isThreatenedWhite) =
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
                                   isThreatenedWhite == True &&
                                   scoreValueBlack > 0 &&
                                   scoreValueBlack < maxBound &&
                                   isThreatenedBlack == False
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

    (scoreValueBlack, isThreatenedBlack, scoreValueWhite, isThreatenedWhite) =
        getValues board

prop_boardWhereBlackIsCheckmated :: Bool
prop_boardWhereBlackIsCheckmated = scoreValueBlack == minBound &&
                                   isThreatenedBlack == True &&
                                   scoreValueWhite > 0 &&
                                   scoreValueWhite < maxBound &&
                                   isThreatenedWhite == False
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

    (scoreValueBlack, isThreatenedBlack, scoreValueWhite, isThreatenedWhite) =
        getValues board

prop_boardWhereWhiteCantMove :: Bool
prop_boardWhereWhiteCantMove = scoreValueWhite == 0 &&
                               isThreatenedWhite == False &&
                               scoreValueBlack > 0 &&
                               scoreValueBlack < maxBound &&
                               isThreatenedBlack == False
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

    (scoreValueBlack, isThreatenedBlack, scoreValueWhite, isThreatenedWhite) =
        getValues board

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

getValues :: Board -> (Int, Bool, Int, Bool)
getValues board = (scoreValueBlack, isThreatenedBlack, scoreValueWhite, isThreatenedWhite)
    where
        (scoreValueBlack, isThreatenedBlack, _MovesBlack) = score $ setTurn Black board
        (scoreValueWhite, isThreatenedWhite, _MovesWhite) = score $ setTurn White board


return []
runTests = $quickCheckAll
