
module Moves.Common
( MovesFun
)
where

import Types

type MovesFun = Color -> Board -> [Move]
