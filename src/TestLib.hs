
module TestLib
( removeKing
, setEmpty
)
where

import Board

setEmpty :: [Pos] -> Board -> Board
setEmpty [] b = b
setEmpty (p:ps) b = setEmpty ps $ setB p Empty b

removeKing :: Color -> Board -> Board
removeKing color = mapB f
    where
        f square = if square == Piece color King then Empty else square
