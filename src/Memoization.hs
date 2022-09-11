
module Memoization
( new
, eval
)
where

import Data.Map as M

type Fun a b = (a -> b)

data Memo a b = Memo (M.Map a b) (Fun a b)

new :: (Fun a b) -> Memo a b
new f = Memo M.empty f

eval :: (Ord a) => a -> Memo a b -> (Memo a b, b)
eval arg memo@(Memo map fun) =
    case M.lookup arg map of
        (Just val) ->
            (memo, val)
        Nothing ->
            let val = fun arg
                newMap = M.insert arg val map
            in (Memo newMap fun, val)

instance (Eq a, Eq b) => Eq (Memo a b) where
    (Memo map1 _f1) == (Memo map2 _f2) = map1 == map2
