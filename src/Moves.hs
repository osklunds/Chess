
-- Generation of all possible moves.

module Moves
( movesFun
)
where

import Moves.Common
import qualified Moves.Naive

movesFun :: MovesFun
movesFun = Moves.Naive.movesFun
