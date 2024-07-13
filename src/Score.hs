
{-# LANGUAGE TemplateHaskell #-}

module Score
( score
, Result(..)
)
where

import Data.List
import Language.Haskell.TH.Syntax (lift)

import Types
import Moves
import Moves.Naive.CheckAware (threatensKing)
import Score.PieceSquareTable

-- By keeping track of if found kings, a speed improvement was observed
data FoldState = FoldState { scoreValue :: Int
                           , foundBlackKing :: Bool
                           , foundWhiteKing :: Bool
                           }
                 
data Result = Normal Bool
            | Draw
            | Checkmate
            deriving (Eq, Show)

score :: Board -> (Int, Result)
score board = (score, result)
    where
        canMove = not $ null $ movesFun board
        isThreatened = threatensKing $ invertTurn board
        scoreValue = calculateScore board
        -- isThreatened is expensive, so include it as a field that
        -- only is lazily calculated if needed, i.e. by Cli
        (score, result) = case canMove of
                            True ->
                                (scoreValue, Normal isThreatened)
                            False ->
                                case isThreatened of
                                    False ->
                                        (0, Draw)
                                    True ->
                                        (if getTurn board == Black
                                             then maxBound
                                             else minBound, Checkmate)

-- Positive means White is leading, negative means Black is leading
-- (Talk about controversy :D)
calculateScore :: Board -> Int
calculateScore board
   -- Test all combinations of black and white king existing
   -- TODO: Should never happen that not 1 of each king. Assert
   -- and fix board generation
   | scoreSum > 100000 = maxBound
   | scoreSum < -100000 = minBound
   | otherwise      = scoreSum
   where
       (scoreSum, []) = foldB foldFunction (0, allPieceSquareTables') board

-- TODO: Is there a way to make this fast and better looking?
   
foldFunction :: (Int, [((Int,Int,Int,Int,Int,Int), (Int,Int,Int,Int,Int,Int), Int)]) ->
       Square ->
       (Int, [((Int,Int,Int,Int,Int,Int), (Int,Int,Int,Int,Int,Int), Int)])
foldFunction (accScore, (((wp, wn, wb, wr, wq, wk), (bp, bn, bb, br, bq, bk), e)):rest)
    square = (accScore+pstScore+materialScore, rest)
    where
        pstScore = case square of
                       (Piece White Pawn)   -> wp
                       (Piece White Knight) -> wn
                       (Piece White Bishop) -> wb
                       (Piece White Rook)   -> wr
                       (Piece White Queen)  -> wq
                       (Piece White King)   -> wk
                       (Piece Black Pawn)   -> bp
                       (Piece Black Knight) -> bn
                       (Piece Black Bishop) -> bb
                       (Piece Black Rook)   -> br
                       (Piece Black Queen)  -> bq
                       (Piece Black King)   -> bk
                       Empty                -> e
        materialScore = materialValue square

materialValue :: Square -> Int
materialValue (Piece White Pawn) = 100
materialValue (Piece White Knight) = 320
materialValue (Piece White Bishop) = 330
materialValue (Piece White Rook) = 500
materialValue (Piece White Queen) = 900
materialValue (Piece White King) = 200000
materialValue (Piece Black p) = -(materialValue $ Piece White p)
materialValue Empty = 0

allPieceSquareTables' :: [((Int,Int,Int,Int,Int,Int), (Int,Int,Int,Int,Int,Int), Int)]
allPieceSquareTables' = $(lift (allPieceSquareTables))

-- Numerical values from
-- https://www.chessprogramming.org/Simplified_Evaluation_Function
