
module Moves.Naive
( moves
)
where

import Moves.Common
import qualified Moves.Naive.CheckAware

moves :: MovesFun
moves = Moves.Naive.CheckAware.movesForColor
