
{-# LANGUAGE TemplateHaskell #-}

module MoveSelection.Tests where

import Test.QuickCheck

import Board as B
import MoveSelection


test = newBoard
  where
    board    = defaultBoard
    depth    = 5
    move     = moveColor depth Black board
    newBoard = applyMove move board







return []
runTests = $quickCheckAll