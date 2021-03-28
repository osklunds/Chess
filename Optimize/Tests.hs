
{-# LANGUAGE TemplateHaskell #-}

module Optimize.Tests where

import Test.QuickCheck
import System.Random

import Optimize.MiniMax as MM
import Optimize.AlphaBeta as AB


--------------------------------------------------------------------------------
-- Fixed
--------------------------------------------------------------------------------

{-
            n
            10
           ----

    x                       y
    21                      20
   ----

a       b             c             d
30      31            32            33
                     ----

       q   r         s   t   u       v   w
        41  49        42  43  44      45  46
                           ---- 
-}

prop_fixed1 :: Property
prop_fixed1 = counterexample errorString result
  where
    result = AB.optimizeWithSc genSts evalSt 0 initSt == (10,"n") &&
             AB.optimizeWithSc genSts evalSt 1 initSt == (21,"x") &&
             AB.optimizeWithSc genSts evalSt 2 initSt == (32,"y") &&
             AB.optimizeWithSc genSts evalSt 3 initSt == (44,"y")
    errorString = show $ AB.optimizeWithSc genSts evalSt 3 initSt

    initSt = "n"

    genSts "n" = ["x","y"]
    genSts "x" = ["a","b"]
    genSts "y" = ["c","d"]
    genSts "a" = [] -- ["p"]
    genSts "b" = ["q","r"]
    genSts "c" = ["s","t","u"]
    genSts "d" = ["v","w"]

    evalSt "n" = 10 :: Int
    evalSt "x" = 21
    evalSt "y" = 20
    evalSt "a" = 30
    evalSt "b" = 31
    evalSt "c" = 32
    evalSt "d" = 33
    evalSt "p" = 40
    evalSt "q" = 41
    evalSt "r" = 49
    evalSt "s" = 42
    evalSt "t" = 43
    evalSt "u" = 44
    evalSt "v" = 45
    evalSt "w" = 46


--------------------------------------------------------------------------------
-- Arbitrary
--------------------------------------------------------------------------------

type State = [Int]

genSts :: Int -> State -> [State]
genSts n st
  | evalSt st == n-1 = []
  | otherwise        = [(m:st) | m <- [1..n]]

evalSt :: State -> Int
evalSt state = (foldl (+) 0 state + foldl (*) 1 state)

prop_alphaBetaEqualsMiniMax :: Int -> Bool
prop_alphaBetaEqualsMiniMax seed = fst optMiniMax == fst optAlphaBeta
  where
    g            = mkStdGen seed
    (n,g')       = uniformR (1 :: Int, 6) g
    (d,_)        = uniformR (1 :: Int, 6) g'
    initSt       = []
    genF         = genSts n
    evalF        = evalSt

    optMiniMax   = MM.optimizeWithSc genF evalF d initSt
    optAlphaBeta = AB.optimizeWithSc genF evalF d initSt


--------------------------------------------------------------------------------
-- Benchmark
--------------------------------------------------------------------------------

--benchmark :: String
--benchmark = optimize j

--evalBoard



return []
runTests = $quickCheckAll