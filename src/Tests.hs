
module Tests where

import Board.Tests as B
import Moves.Naive.CheckAware.Tests as MNCA
import Moves.Naive.CheckUnaware.Tests as MNCU
import Moves.Naive.NormalMoves.Tests as MNN
import MoveSelection.Tests as MS
import MoveSelection.Score.Tests as S
import Optimize.Tests as O
import GameResult.Tests as GR
import Memoization.Tests as M

runAllTests :: IO Bool
runAllTests = do
  b    <- B.runTests
  mnca <- MNCA.runTests
  mncu <- MNCU.runTests
  mnn  <- MNN.runTests
  ms   <- MS.runTests
  s    <- S.runTests
  o    <- O.runTests
  gr   <- GR.runTests
  m    <- M.runTests

  return $ and [b,mnca,mncu,mnn,ms,s,o,gr,m]
