
module Types.Tests where

import qualified Types.Board.Tests as B

runTests :: IO Bool
runTests = do
    b  <- B.runTests

    return $ and [b]
