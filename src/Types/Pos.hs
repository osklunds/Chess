
module Types.Pos
( Pos(..)
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
