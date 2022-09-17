
import qualified Cli as C
import qualified Board as B

main :: IO ()
main = C.start playAs startBoard

playAs = B.Black

-- Change comments to change what's used
startBoard = Nothing
-- startBoard = Just $ read  "  0 1 2 3 4 5 6 7  \n\
--                           \0                 0\n\
--                           \1 ♔               1\n\
--                           \2                 2\n\
--                           \3                 3\n\
--                           \4                 4\n\
--                           \5           ♟ ♚   5\n\
--                           \6        [ ]      6\n\
--                           \7                 7\n\
--                           \  0 1 2 3 4 5 6 7"
