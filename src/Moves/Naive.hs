
module Moves.Naive
( movesF
)
where

import Moves.Common
import qualified Moves.Naive.CheckAware

movesF :: MovesFun
movesF = Moves.Naive.CheckAware.movesF
