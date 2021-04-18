
module Moves
( movesForColor
)
where

import Board as B
import qualified Moves.CheckUnawareMoves as CU


movesForColor :: Color -> Board -> [((Int,Int),(Int,Int))]
movesForColor = CU.movesForColor
-- TODO: Take check into account