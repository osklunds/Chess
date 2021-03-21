
{-# LANGUAGE ConstraintKinds #-}

module Optimize.AlphaBeta
( optimize
, optimizeWithSc
)
where

import Data.Maybe

optimize :: (Score sc, Integral d) =>
            (st -> [st]) -> (st -> sc) -> d -> st -> st
optimize genSts evalSt d initSt = fst $ optimizeWithSc genSts evalSt d initSt

{-
function alphabeta(node, depth, α, β, maximizingPlayer) is
    if depth = 0 or node is a terminal node then
        return the heuristic value of node
    if maximizingPlayer then
        value := −∞
        for each child of node do
            value := max(value, alphabeta(child, depth − 1, α, β, FALSE))
            α := max(α, value)
            if α ≥ β then
                break (* β cutoff *)
        return value
    else
        value := +∞
        for each child of node do
            value := min(value, alphabeta(child, depth − 1, α, β, TRUE))
            β := min(β, value)
            if β ≤ α then
                break (* α cutoff *)
        return value

(* Initial call *)
alphabeta(origin, depth, −∞, +∞, TRUE)
-}

type Score a = (Ord a, Num a, Bounded a)

type SearchFun st sc d = (st -> [st]) ->
                         (st -> sc) ->
                         d ->
                         sc ->
                         sc ->
                         [st] ->
                         (st,sc)

optimizeWithSc :: (Score sc, Integral d) =>
            (st -> [st]) -> (st -> sc) -> d -> st -> (st,sc)
optimizeWithSc genSts evalSt d st =
  alphabeta genSts evalSt d minBound maxBound maxSearch st

alphabeta :: (Score sc, Integral d) => (st -> [st]) ->
                                       (st -> sc) ->
                                       d ->
                                       sc ->
                                       sc ->
                                       SearchFun st sc d ->
                                       st ->
                                       (st,sc)
alphabeta _genSts evalSt 0 _a _b _searchFun st = (st, evalSt st)
alphabeta  genSts evalSt d  a  b  searchFun st =
  searchFun genSts evalSt d a b (genSts st)

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