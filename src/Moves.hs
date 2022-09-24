
-- Generation of all possible moves.

module Moves
( movesForColor
)
where

import Moves.Common
import qualified Moves.Naive as N

movesForColor :: MovesFun
movesForColor = N.movesForColor
