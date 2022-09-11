
{-# LANGUAGE TemplateHaskell #-}

module Optimize.Tests where

import Test.QuickCheck
import System.Random
import Data.List
import Control.Parallel

import Optimize.MiniMax as MM
import Optimize.MiniMaxPar as MMP
import Optimize.AlphaBeta as AB
import Optimize.AlphaBetaMemo as ABM


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

            aa  bb   cc dd   ee ff        gg hh         ii jj
            51  51   53 54   59 60        55 56         57 58
                                                       ----
-}

prop_fixed1 :: Bool
prop_fixed1 = all check optFuns
  where
    check optFun = optFun genSts evalSt 0 initSt == (10,"n") &&
                   optFun genSts evalSt 1 initSt == (21,"x") &&
                   optFun genSts evalSt 2 initSt == (32,"y") &&
                   optFun genSts evalSt 3 initSt == (44,"y") &&
                   optFun genSts evalSt 4 initSt == (57,"y")

    initSt = "n"

    genSts "n" = ["x","y"]
    genSts "x" = ["a","b"]
    genSts "y" = ["c","d"]
    genSts "a" = []
    genSts "b" = ["q","r"]
    genSts "c" = ["s","t","u"]
    genSts "d" = ["v","w"]
    genSts "q" = ["aa","bb"]
    genSts "r" = ["cc","dd"]
    genSts "s" = ["ee","ff"]
    genSts "t" = []
    genSts "u" = ["gg","hh"]
    genSts "v" = []
    genSts "w" = ["ii","jj"]

    evalSt "n"  = 10 :: Int
    evalSt "x"  = 21
    evalSt "y"  = 20
    evalSt "a"  = 30
    evalSt "b"  = 31
    evalSt "c"  = 32
    evalSt "d"  = 33
    evalSt "p"  = 40
    evalSt "q"  = 41
    evalSt "r"  = 49
    evalSt "s"  = 42
    evalSt "t"  = 43
    evalSt "u"  = 44
    evalSt "v"  = 45
    evalSt "w"  = 46
    evalSt "aa" = 51
    evalSt "bb" = 52
    evalSt "cc" = 53
    evalSt "dd" = 54
    evalSt "ee" = 59
    evalSt "ff" = 60
    evalSt "gg" = 55
    evalSt "hh" = 56
    evalSt "ii" = 57
    evalSt "jj" = 58


--------------------------------------------------------------------------------
-- Arbitrary
--------------------------------------------------------------------------------

type State = [Int]

genSts :: Int -> State -> [State]
genSts n st
  | evalSt st `mod` n == 0 = []
  | otherwise              = [(m:st) | m <- [1..n]]

evalSt :: State -> Int
evalSt state = (foldl (+) 0 state + foldl (*) 1 state)

prop_equalsMiniMax :: Int -> Bool
prop_equalsMiniMax seed = all (== (head optVals)) optVals
  where
    g       = mkStdGen seed
    (n,g')  = uniformR (1 :: Int, 6) g
    (d,_)   = uniformR (1 :: Int, 6) g'
    initSt  = []
    genF    = genSts n
    evalF   = evalSt
    optVals = map (\optFun -> fst $ optFun genF evalF d initSt) optFuns


--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

optFuns = [MM.optimizeWithSc,
           MMP.optimizeWithSc,
           AB.optimizeWithSc,
           ABM.optimizeWithSc]


return []
runTests = $quickCheckAll