
module Types.Pos
( Pos(..)
, rowOf
, colOf
, isWithinBoard
)
where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

data Pos = Pos Int Int
         deriving (Eq, Show, Ord)

instance Arbitrary Pos where
    arbitrary = do
        row <- elements [0..7]
        col <- elements [0..7]
        return $ Pos row col

rowOf :: Pos -> Int
rowOf (Pos row _col) = row

colOf :: Pos -> Int
colOf (Pos _row col) = col

isWithinBoard :: Pos -> Bool
isWithinBoard (Pos row col) = 0 <= row && row < 8 && 0 <= col && col < 8

