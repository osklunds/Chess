
module Moves.Naive.TestLib
( verifyMoves
)
where

import Test.QuickCheck
import Data.List

import Types
import Moves.Common
import Lib

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

verifyMoves :: (Ord move, Show move, Show board) =>
              (color -> board -> [move]) -> [move] -> color -> board -> Property
verifyMoves movesF expMoves' color board =
    counterexample errorString verificationResult
    where
        expMoves           = sort expMoves'
        actMoves           = sort $ movesF color board
        verificationResult = expMoves == actMoves
        actualMissing      = expMoves \\ actMoves
        actualExtra        = actMoves \\ expMoves
        errorString        = show board ++ "\n" ++
                             "Expected moves: " ++ show expMoves ++ "\n\n" ++
                             "Actual moves: " ++ show actMoves ++ "\n\n" ++
                             "Actual is missing: " ++ show actualMissing ++ "\n\n" ++
                             "Actual has these extra: " ++ show actualExtra

--------------------------------------------------------------------------------
-- Black and White give the same moves
--------------------------------------------------------------------------------
