module Optimize.MiniMax
    ( optimize
    , optimizeWithSc
    ) where

import Optimize.Score

optimize ::
       (Score sc, Integral d) => (st -> [st]) -> (st -> sc) -> d -> st -> st
optimize genF evalF d st = snd $ optimizeWithSc genF evalF d st

optimizeWithSc ::
       (Score sc, Integral d)
    => (st -> [st])
    -> (st -> sc)
    -> d
    -> st
    -> (sc, st)
optimizeWithSc genF evalF d st = maxi genF evalF d st

maxi ::
       (Score sc, Integral d)
    => (st -> [st])
    -> (st -> sc)
    -> d
    -> st
    -> (sc, st)
maxi genF evalF d st
    | d == 0 || null sts = (evalF st, st)
    | otherwise = foldl1 maxScAndSt scsAndSts
  where
    sts = genF st
    scs = map (fst . mini genF evalF (d - 1)) sts
    scsAndSts = zip scs sts
    maxScAndSt x y
        | fst x > fst y = x
        | otherwise = y

mini ::
       (Score sc, Integral d)
    => (st -> [st])
    -> (st -> sc)
    -> d
    -> st
    -> (sc, st)
mini genF evalF d st
    | d == 0 || null sts = (evalF st, st)
    | otherwise = foldl1 minScAndSt scsAndSts
  where
    sts = genF st
    scs = map (fst . maxi genF evalF (d - 1)) sts
    scsAndSts = zip scs sts
    minScAndSt x y
        | fst x < fst y = x
        | otherwise = y
