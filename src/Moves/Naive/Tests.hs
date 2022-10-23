
module Moves.Naive.Tests where

import qualified Moves.Naive.CheckAware.Tests as CA
import qualified Moves.Naive.CheckUnaware.Tests as CU
import qualified Moves.Naive.NormalMoves.Tests as N

runTests :: IO Bool
runTests = do
  ca <- CA.runTests
  cu <- CU.runTests
  n  <- N.runTests

  return $ and [ca,cu,n]
