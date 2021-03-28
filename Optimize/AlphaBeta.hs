
{-# LANGUAGE ConstraintKinds #-}

module Optimize.AlphaBeta
( optimize
, optimizeWithSc
)
where

import Data.Maybe
import Optimize.Score


optimize :: (Score sc, Integral d) =>
            (st -> [st]) -> (st -> sc) -> d -> st -> st
optimize genSts evalSt d initSt = snd $ optimizeWithSc genSts evalSt d initSt

optimizeWithSc :: (Score sc, Integral d) =>
            (st -> [st]) -> (st -> sc) -> d -> st -> (sc,st)
optimizeWithSc genSts evalSt d st = (sc,st1)
  where
    (st1,sc) = alphabeta genSts evalSt d minBound maxBound maxSearch st


type SearchFun st sc d = (st -> [st]) ->
                         (st -> sc) ->
                         d ->
                         sc ->
                         sc ->
                         [st] ->
                         (st,sc)


alphabeta :: (Score sc, Integral d) => (st -> [st]) ->
                                       (st -> sc) ->
                                       d ->
                                       sc ->
                                       sc ->
                                       SearchFun st sc d ->
                                       st ->
                                       (st,sc)
alphabeta  genSts evalSt d  a  b  searchFun st
  | d == 0 || null sts = (st, evalSt st)
  | otherwise          = searchFun genSts evalSt d a b sts
  where
    sts = genSts st

maxSearch :: (Score sc, Integral d) => SearchFun st sc d
maxSearch = maxSearch' (Nothing, minBound)

maxSearch' :: (Score sc, Integral d) => (Maybe st, sc) -> SearchFun st sc d
maxSearch' (maxSt, maxSc) genSts evalSt d a b (st:sts)
  | newA >= b = (fromJust newMaxSt, newMaxSc)
  | null sts  = (fromJust newMaxSt, newMaxSc)
  | otherwise = maxSearch' (newMaxSt,newMaxSc) genSts evalSt d newA b sts
  where
    (_thisSt, thisSc) = alphabeta genSts evalSt (d-1) a b minSearch st
    (newMaxSt, newMaxSc) = case thisSc > maxSc of
                              True  -> (Just st, thisSc)
                              False -> (maxSt, maxSc)
    newA                 = max a newMaxSc

minSearch :: (Score sc, Integral d) => SearchFun st sc d
minSearch = minSearch' (Nothing, maxBound)

minSearch' :: (Score sc, Integral d) => (Maybe st, sc) -> SearchFun st sc d
minSearch' (minSt,minSc) genSts evalSt d a b (st:sts)
  | newB <= a = (fromJust newMinSt, newMinSc)
  | null sts  = (fromJust newMinSt, newMinSc)
  | otherwise = minSearch' (newMinSt,newMinSc) genSts evalSt d a newB sts
  where
    (_thisSt, thisSc) = alphabeta genSts evalSt (d-1) a b maxSearch st
    (newMinSt, newMinSc) = case thisSc < minSc of
                              True  -> (Just st, thisSc)
                              False -> (minSt, minSc)
    newB                 = min b newMinSc