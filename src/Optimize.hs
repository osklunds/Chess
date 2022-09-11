
-- General function for optimization using Alpha Beta pruning.

{-# LANGUAGE ConstraintKinds #-}

module Optimize
( optimize
)
where

import qualified Optimize.AlphaBeta as AB
import qualified Optimize.MiniMax as MM
import Optimize.Score

optimize :: (Score sc, Integral d) =>
            (st -> [st]) -> (st -> sc) -> d -> st -> st
optimize = AB.optimize