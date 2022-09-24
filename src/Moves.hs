
-- Generation of all possible moves.

module Moves
( movesF
)
where

import Moves.Common
import qualified Moves.Naive as N

movesF :: MovesFun
movesF = N.movesF
