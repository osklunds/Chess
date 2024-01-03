
module Tests where

import qualified Types.Tests as T
import qualified Moves.Tests as MO
import qualified Score.Tests as S
import qualified MoveSelection.Tests as MS
import qualified Optimize.Tests as O
import qualified GameResult.Tests as GR
import qualified Memoization.Tests as ME

runTests :: IO Bool
runTests = do
  t  <- T.runTests
  mo <- MO.runTests
  s  <- S.runTests
  ms <- MS.runTests
  o  <- O.runTests
  gr <- GR.runTests
  me <- ME.runTests

  return $ and [t,mo,s,ms,o,gr,me]
