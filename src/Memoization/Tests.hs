

{-# LANGUAGE TemplateHaskell #-}

module Memoization.Tests where

import Test.QuickCheck
import Test.QuickCheck.Monadic

import qualified Memoization as M

newtype Return = Return Int
               deriving (Eq)

prop_eval :: Int -> Property
prop_eval arg = monadicIO $ do
    -- Arrange
    let memo1 = M.new fun

    -- Act
    let (memo2, ret) = M.eval arg memo1

    -- Assert
    assert $ memo1 /= memo2
    assert $ ret == Return arg

fun arg = Return arg



return []
runTests = $quickCheckAll