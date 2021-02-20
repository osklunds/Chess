
{-# LANGUAGE TemplateHaskell #-} 

module Board.Tests
(
)
where

import Test.QuickCheck

import Board

--------------------------------------------------------------------------------
-- get
--------------------------------------------------------------------------------

prop_getTopleft :: Bool
prop_getTopleft = get (0,0) defaultBoard == Piece Black Rook

prop_getBottomRight :: Bool
prop_getBottomRight = get (7,7) defaultBoard == Piece White Rook

prop_getMiddle :: Bool
prop_getMiddle = get (4,4) defaultBoard == Empty

--------------------------------------------------------------------------------
-- set
--------------------------------------------------------------------------------

prop_set :: (Int,Int) -> Square -> Board -> Bool
prop_set (row,col) sq board = get pos newBoard == sq &&
                              equalExcept board newBoard pos
  where
    row' = row `mod` 8
    col' = col `mod` 8
    pos = (row',col')
    newBoard = set pos sq board

equalExcept :: Board -> Board -> (Int,Int) -> Bool
equalExcept board1 board2 pos =
  all (\p -> get p board1 == get p board2) otherPositions
  where
    allPositions = [(row,col) | row <- [0..7], col <- [0..7]]
    otherPositions = filter ((/=) pos) allPositions

--------------------------------------------------------------------------------
-- misc
--------------------------------------------------------------------------------

prop_showRead :: Board -> Bool
prop_showRead board = board == read (show board)

return []
runTests = $quickCheckAll
