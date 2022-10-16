
module Types.Move
( Move(..)
, Side(..)

, isCastle
)
where

import Types.Pos
import Types.Square

-- Idea: KingMove, QueenMove, etc
-- So that syntactically invalid moves are impossible to create, or
-- are asserted (the latter for e.g. empty squares during castling).
data Move = NormalMove Pos Pos
          | Promote Pos Kind
          | Castle Color Side
          | EnPassant Pos Pos  -- TODO
          deriving (Eq, Ord, Show)

data Side = KingSide
          | QueenSide
          deriving (Eq, Ord, Show)

isCastle :: Move -> Bool
isCastle (Castle _color _side) = True
isCastle _                     = False
