
{-# LANGUAGE ConstraintKinds #-}

module Optimize
( optimize
)
where

import qualified Optimize.MiniMax as MM
import qualified Optimize.AlphaBeta as AB
import qualified Optimize.AlphaBeta as ABS
import Optimize.Score

optimize :: (Score sc, Integral d) =>
            (st -> [st]) -> (st -> sc) -> d -> st -> st
optimize = ABS.optimize