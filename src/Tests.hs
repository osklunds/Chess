
module Tests where

import Board.Tests as B
import Moves.Tests as M
import Moves.CheckUnawareMoves.Tests as CU
import MoveSelection.Tests as MS
import MoveSelection.Score.Tests as S
import Optimize.Tests as O

runAllTests :: IO Bool
runAllTests = do
  b  <- B.runTests
  m  <- M.runTests
  cu <- CU.runTests
  ms <- MS.runTests
  s  <- S.runTests
  o  <- O.runTests

  return $ and [b,m,cu,ms,s,o]
