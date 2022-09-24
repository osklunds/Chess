
module Lib
( isSubsetOf
)
where

import Data.List


isSubsetOf :: (Eq a) => [a] -> [a] -> Bool
xs `isSubsetOf` ys = null $ xs \\ ys
