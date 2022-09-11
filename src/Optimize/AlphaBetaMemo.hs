
{-# LANGUAGE ConstraintKinds #-}

module Optimize.AlphaBetaMemo
( optimizeWithSc
)
where

import Data.Maybe

import Optimize.Types
import qualified Memoization as M


type Memo st = M.Memo st [st]

type SearchFun sc st d = Memo st ->
                         EvalFun st sc ->
                         d ->
                         sc ->
                         sc ->
                         [st] ->
                         (sc, st, Memo st)

optimizeWithSc :: (State st, Score sc, Integral d) => OptFunSc st sc d
optimizeWithSc genF evalF d initSt = (sc,st)
    where
        memo = M.new genF
        (sc,st,_memo) = alphabeta memo evalF d minBound maxBound maxSearch initSt

alphabeta :: (Score sc, Integral d, Ord st) => Memo st ->
                                               EvalFun st sc ->
                                               d ->
                                               sc ->
                                               sc ->
                                               SearchFun sc st d ->
                                               st ->
                                               (sc, st, Memo st)
alphabeta memo evalF d  a  b  searchFun st
  | d == 0    = (evalF st, st, memo)
  | null sts  = (evalF st, st, memo')
  | otherwise = searchFun memo' evalF d a b sts
  where
    (memo', sts) = M.eval st memo

maxSearch :: (Score sc, Integral d, Ord st) => SearchFun sc st d
maxSearch = maxSearch' (minBound, Nothing)

maxSearch' :: (Score sc, Integral d, Ord st) => (sc, Maybe st) -> SearchFun sc st d
maxSearch' (maxSc,maxSt) memo evalF d a b (st:sts)
  | newA >= b = (newMaxSc, fromJust newMaxSt, memo')
  | null sts  = (newMaxSc, fromJust newMaxSt, memo')
  | otherwise = maxSearch' (newMaxSc,newMaxSt) memo' evalF d newA b sts
  where
    (thisSc, _thisSt, memo') = alphabeta memo evalF (d-1) a b minSearch st
    (newMaxSc, newMaxSt) = case thisSc > maxSc  || isNothing maxSt of
                              True  -> (thisSc, Just st)
                              False -> (maxSc, maxSt)
    newA                 = max a newMaxSc

minSearch :: (Score sc, Integral d, Ord st) => SearchFun sc st d
minSearch = minSearch' (maxBound, Nothing)

minSearch' :: (Score sc, Integral d, Ord st) => (sc, Maybe st) -> SearchFun sc st d
minSearch' (minSc,minSt) memo evalF d a b (st:sts)
  | newB <= a = (newMinSc, fromJust newMinSt, memo')
  | null sts  = (newMinSc, fromJust newMinSt, memo')
  | otherwise = minSearch' (newMinSc,newMinSt) memo' evalF d a newB sts
  where
    (thisSc, _thisSt, memo') = alphabeta memo evalF (d-1) a b maxSearch st
    (newMinSc, newMinSt) = case thisSc < minSc || isNothing minSt of
                              True  -> (thisSc, Just st)
                              False -> (minSc, minSt)
    newB                 = min b newMinSc
