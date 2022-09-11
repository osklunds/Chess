
-- Generation of all possible moves.

-- The long term goal is that this module is so fast that CheckUnaware is
-- not needed.

module Moves
( movesForColor
)
where

import Board
import qualified Moves.CheckAware


movesForColor :: Color -> Board -> [((Int,Int),(Int,Int))]
movesForColor = Moves.CheckAware.movesForColor
