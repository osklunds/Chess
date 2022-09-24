
module Moves.TestLib
( verifyMoves
)
where

import Test.QuickCheck
import Data.List

import Board

verifyMoves :: [Move] -> Color -> Board -> (Color -> Board -> [Move]) -> Property
verifyMoves expMoves' color board movesFun =
    counterexample errorString verificationResult
    where
        expMoves           = sort expMoves'
        actMoves           = sort $ movesFun color board
        verificationResult = expMoves == actMoves
        actualMissing      = expMoves \\ actMoves
        actualExtra        = actMoves \\ expMoves
        errorString        = show board ++ "\n" ++
                             "Expected moves: " ++ show expMoves ++ "\n\n" ++
                             "Actual moves: " ++ show actMoves ++ "\n\n" ++
                             "Actual is missing: " ++ show actualMissing ++ "\n\n" ++
                             "Actual has these extra: " ++ show actualExtra
