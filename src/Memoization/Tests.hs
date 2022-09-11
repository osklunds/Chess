

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
    assert $ ret == fun arg

prop_eval_twice :: Int -> Int -> Property
prop_eval_twice arg1 arg2 = arg1 /= arg2 ==> monadicIO $ do
    -- Arrange
    let memo1 = M.new fun
    let (memo2, _ret1) = M.eval arg1 memo1

    -- Act
    let (memo3, ret2) = M.eval arg2 memo2

    -- Assert
    let (_memo4, ret3) = M.eval arg1 memo3

    assert $ memo2 /= memo3
    assert $ ret2 == fun arg2
    assert $ ret3 == fun arg1






fun arg = Return arg



return []
runTests = $quickCheckAll