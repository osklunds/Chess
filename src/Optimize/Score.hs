{-# LANGUAGE ConstraintKinds #-}

module Optimize.Score
    ( Score
    ) where

type Score a = (Ord a, Bounded a)
