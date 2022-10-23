
module Tests where

import qualified Types.Tests as T
import qualified Moves.Tests as MO
import qualified MoveSelection.Tests as MS
import qualified Optimize.Tests as O
import qualified GameResult.Tests as GR
import qualified Memoization.Tests as ME

runTests :: IO Bool
runTests = do
  t  <- T.runTests
  mo <- MO.runTests
  ms <- MS.runTests
  o  <- O.runTests
  gr <- GR.runTests
  me <- ME.runTests

  return $ and [t,mo,ms,o,gr,me]
