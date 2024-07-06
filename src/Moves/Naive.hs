
module Moves.Naive
( movesFun
)
where

import Data.MemoTrie

import Moves.Common
import qualified Moves.Naive.CheckAware

movesFun :: MovesFun
movesFun = Moves.Naive.CheckAware.movesFun
