
module Tests where

import Board.Tests as B
import Moves.CheckAware.Tests as MCA
import Moves.CheckUnaware.Tests as MCU
import MoveSelection.Tests as MS
import MoveSelection.Score.Tests as S
import Optimize.Tests as O
import GameResult.Tests as GR

runAllTests :: IO Bool
runAllTests = do
  b   <- B.runTests
  mca <- MCA.runTests
  mcu <- MCU.runTests
  ms  <- MS.runTests
  s   <- S.runTests
  o   <- O.runTests
  gr  <- GR.runTests

  return $ and [b,mca,mcu,ms,s,o,gr]
