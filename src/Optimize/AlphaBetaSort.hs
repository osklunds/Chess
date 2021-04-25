
{-# LANGUAGE ConstraintKinds #-}

module Optimize.AlphaBetaSort
( optimize
, optimizeWithSc
)
where

import Data.Maybe
import Data.List

import Optimize.Score


optimize :: (Score sc, Integral d) =>
            (st -> [st]) -> (st -> sc) -> d -> st -> st
optimize genF evalF d st = snd $ optimizeWithSc genF evalF d st

optimizeWithSc :: (Score sc, Integral d) =>
            (st -> [st]) -> (st -> sc) -> d -> st -> (sc,st)
optimizeWithSc genF evalF d st =
  alphabeta genF evalF d minBound maxBound maxSearch st


type SearchFun sc st d = (st -> [st]) ->
                         (st -> sc) ->
                         d ->
                         sc ->
                         sc ->
                         [st] ->
                         (sc,st)


alphabeta :: (Score sc, Integral d) => (st -> [st]) ->
                                       (st -> sc) ->
                                       d ->
                                       sc ->
                                       sc ->
                                       SearchFun sc st d ->
                                       st ->
                                       (sc,st)
alphabeta genF evalF d  a  b  searchFun st
  | d == 0 || null sts = (evalF st, st)
  | otherwise          = searchFun genF evalF d a b sts

maxSearch :: (Score sc, Integral d) => SearchFun sc st d
maxSearch genF evalF d a b sts =
  maxSearch' (minBound, Nothing) genF evalF d a b sts'
  where
    scsAndSts  = map (\st -> (evalF st, st)) sts
    scsAndSts' = sortBy (\(sc1,_) (sc2,_) -> compare sc2 sc1) scsAndSts
    sts'       = map snd scsAndSts'

maxSearch' :: (Score sc, Integral d) => (sc, Maybe st) -> SearchFun sc st d
maxSearch' (maxSc,maxSt) genF evalF d a b (st:sts)
  | newA >= b = (newMaxSc, fromJust newMaxSt)
  | null sts  = (newMaxSc, fromJust newMaxSt)
  | otherwise = maxSearch' (newMaxSc,newMaxSt) genF evalF d newA b sts
  where
    (thisSc, _thisSt)    = alphabeta genF evalF (d-1) a b minSearch st
    (newMaxSc, newMaxSt) = case thisSc > maxSc  || isNothing maxSt of
                              True  -> (thisSc, Just st)
                              False -> (maxSc, maxSt)
    newA                 = max a newMaxSc

minSearch :: (Score sc, Integral d) => SearchFun sc st d
minSearch genF evalF d a b sts =
  minSearch' (maxBound, Nothing) genF evalF d a b sts'
  where
    scsAndSts  = map (\st -> (evalF st, st)) sts
    scsAndSts' = sortBy (\(sc1,_) (sc2,_) -> compare sc1 sc2) scsAndSts
    sts'       = map snd scsAndSts'

minSearch' :: (Score sc, Integral d) => (sc, Maybe st) -> SearchFun sc st d
minSearch' (minSc,minSt) genF evalF d a b (st:sts)
  | newB <= a = (newMinSc, fromJust newMinSt)
  | null sts  = (newMinSc, fromJust newMinSt)
  | otherwise = minSearch' (newMinSc,newMinSt) genF evalF d a newB sts
  where
    (thisSc, _thisSt)    = alphabeta genF evalF (d-1) a b maxSearch st
    (newMinSc, newMinSt) = case thisSc < minSc || isNothing minSt of
                              True  -> (thisSc, Just st)
                              False -> (minSc, minSt)
    newB                 = min b newMinSc