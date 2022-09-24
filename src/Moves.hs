
-- Generation of all possible moves.

module Moves
( moves
)
where

import Moves.Common
import qualified Moves.Naive as N

moves :: MovesFun
moves = N.moves
