
{-# LANGUAGE TemplateHaskell #-}

module Moves.Tests where


--------------------------------------------------------------------------------
-- Fixed boards
--------------------------------------------------------------------------------

prop_fixedBoard1 :: Property
prop_fixedBoard1 = verifyMoves Black board moves
  where
    board = read  "  0 1 2 3 4 5 6 7  \n\
                  \0 ♟         ♚   ♝ 0\n\
                  \1   ♞           ♝ 1\n\
                  \2   ♔     ♝       2\n\
                  \3     ♜   ♙     ♙ 3\n\
                  \4     ♛   ♖ ♟ ♗ ♚ 4\n\
                  \5     ♖       ♗ ♜ 5\n\
                  \6       ♘ ♞ ♟ ♝ ♟ 6\n\
                  \7   ♔   ♜   ♞ ♖ ♝ 7\n\
                  \  0 1 2 3 4 5 6 7"



return []
runTests = $quickCheckAll