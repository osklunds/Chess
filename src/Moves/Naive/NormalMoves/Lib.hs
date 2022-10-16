
module Moves.Naive.NormalMoves.Lib
( tupleAdd
, tupleSub
, tupleMul
, tupleSignum
, tupleMaxAbs
, isWithinBoard
, getB
, setB
, toPos
)
where

import Types as T hiding (getB, setB)
import qualified Types as T

tupleAdd :: (Int,Int) -> (Int,Int) -> (Int,Int)
tupleAdd (a,b) (c,d) = (a+c,b+d)

tupleSub :: (Int,Int) -> (Int,Int) -> (Int,Int)
tupleSub (a,b) (c,d) = (a-c,b-d)

tupleMul :: (Int,Int) -> Int -> (Int,Int)
tupleMul (a,b) c = (a*c,b*c)

tupleSignum :: (Int,Int) -> (Int,Int)
tupleSignum (a,b) = (signum a,signum b)

tupleMaxAbs :: (Int,Int) -> Int
tupleMaxAbs (a,b) = max (abs a) (abs b)

isWithinBoard :: (Int,Int) -> Bool
isWithinBoard (row,col) = 0 <= row && row < 8 && 0 <= col && col < 8

getB :: (Int,Int) -> Board -> Square
getB = T.getB . toPos

setB :: (Int,Int) -> Square -> Board -> Board
setB = T.setB . toPos

toPos :: (Int,Int) -> Pos
toPos (row,col) = Pos row col
