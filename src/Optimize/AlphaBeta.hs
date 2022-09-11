
{-# LANGUAGE ConstraintKinds #-}

module Optimize.AlphaBeta
( optimizeWithSc
)
where

import Data.Maybe

import Optimize.Types

type SearchFun sc st d = GenFun st ->
                         EvalFun st sc ->
                         d ->
                         sc ->
                         sc ->
                         [st] ->
                         (sc,st)

optimizeWithSc :: (State st, Score sc, Integral d) => OptFunSc st sc d
optimizeWithSc genF evalF d st =
  alphabeta genF evalF d minBound maxBound maxSearch st

alphabeta :: (Score sc, Integral d) => GenFun st ->
                                       EvalFun st sc ->
                                       d ->
                                       sc ->
                                       sc ->
                                       SearchFun sc st d ->
                                       st ->
                                       (sc,st)
alphabeta genF evalF d  a  b  searchFun st
  | d == 0 || null sts = (evalF st, st)
  | otherwise          = searchFun genF evalF d a b sts
  where
    sts = genF st

maxSearch :: (Score sc, Integral d) => SearchFun sc st d
maxSearch = maxSearch' (minBound, Nothing)

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
minSearch = minSearch' (maxBound, Nothing)

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