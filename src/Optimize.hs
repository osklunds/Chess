
-- General function for optimization using Alpha Beta pruning.

{-# LANGUAGE ConstraintKinds #-}

module Optimize
( optimize
)
where

import Optimize.Types

import qualified Optimize.AlphaBeta as AB
import qualified Optimize.AlphaBetaMemo as ABM
import qualified Optimize.MiniMax as MM
import qualified Optimize.MiniMaxPar as MMP

optimize :: (State st, Score sc, Integral d) => OptFun st sc d
optimize genF evalF d st = snd $ AB.optimizeWithSc genF evalF d st
