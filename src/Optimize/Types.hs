
{-# LANGUAGE ConstraintKinds #-}

module Optimize.Types where

type Score a = (Ord a, Bounded a)
type State a = (Ord a)

type EvalFun st sc = (st -> sc)
type GenFun st = (st -> [st])
type OptFun st sc d = GenFun st -> EvalFun st sc -> d -> st -> st
type OptFunSc st sc d = GenFun st -> EvalFun st sc -> d -> st -> (sc,st)
