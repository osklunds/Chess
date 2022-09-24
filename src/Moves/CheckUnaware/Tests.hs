
{-# LANGUAGE TemplateHaskell #-}

module Moves.CheckUnaware.Tests where

import Test.QuickCheck

import Lib
import Board
import Moves.CheckUnaware
import qualified Moves.NormalMoves as NM


--------------------------------------------------------------------------------
-- Arbitrary boards
--------------------------------------------------------------------------------

prop_movesIsSupersetOfNormalMoves :: Color -> Board -> Bool
prop_movesIsSupersetOfNormalMoves color board =
    normalMoves `isSubsetOf` moves
    where
        moves        = movesForColor color board
        normalMoves  = normalMovesForColor color board


normalMovesForColor :: Color -> Board -> [Move]
normalMovesForColor c b = map toNormalMove $ NM.movesForColor c b
    where
        toNormalMove ((rowS,colS),(rowD,colD)) =
            (NormalMove (Pos rowS colS) (Pos rowD colD))


return []
runTests = $quickCheckAll
