
module TestLib
(
)
where

import System.Random
import Test.QuickCheck

instance Arbitrary StdGen where
  arbitrary = do
    seed <- arbitrary
    return $ mkStdGen seed
