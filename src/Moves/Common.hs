
module Moves.Common
( MovesFun
)
where

import Board

type MovesFun = Color -> Board -> [Move]
