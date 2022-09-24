
module Moves.Naive.TestLib
( verifyMoves
)
where

import Test.QuickCheck
import Data.List

import Board
import Moves.Common

verifyMoves :: [Move] -> Color -> Board -> MovesFun -> Property
verifyMoves expMoves' color board movesF =
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
