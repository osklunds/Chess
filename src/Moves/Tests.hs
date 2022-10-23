
module Moves.Tests where

import qualified Moves.Naive.Tests as N

runTests :: IO Bool
runTests = do
  n  <- N.runTests

  return $ and [n]
