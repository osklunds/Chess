module Tests where

import Board.Tests as B
import GameResult.Tests as GR
import MoveSelection.Score.Tests as S
import MoveSelection.Tests as MS
import Moves.CheckAware.Tests as MCA
import Moves.CheckUnaware.Tests as MCU
import Optimize.Tests as O

runAllTests :: IO Bool
runAllTests = do
    b <- B.runTests
    mca <- MCA.runTests
    mcu <- MCU.runTests
    ms <- MS.runTests
    s <- S.runTests
    o <- O.runTests
    gr <- GR.runTests
    return $ and [b, mca, mcu, ms, s, o, gr]
