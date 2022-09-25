
module Lib
( isSubsetOf
, listEq
)
where

import Data.List

isSubsetOf :: (Eq a) => [a] -> [a] -> Bool
xs `isSubsetOf` ys = null $ xs \\ ys

listEq :: (Eq a, Ord a) => [a] -> [a] -> Bool
xs `listEq` ys = sort xs == sort ys
