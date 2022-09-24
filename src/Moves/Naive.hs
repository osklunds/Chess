
module Moves.Naive
( movesForColor
)
where

import Moves.Common
import qualified Moves.Naive.CheckAware

movesForColor :: MovesFun
movesForColor = Moves.Naive.CheckAware.movesForColor
