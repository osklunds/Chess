
{-# LANGUAGE ConstraintKinds #-}

module Optimize.Score
( Score
)
where

type Score a = (Ord a, Num a, Bounded a)