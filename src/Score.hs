
module Score
( score
, Result(..)
)
where

import Data.List

import Types
import Moves
import Moves.Naive.CheckAware (threatensKing)

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
       scores = [scoreForPos (Pos row col) board | row <- [0..7], col <- [0..7]]
       scoreSum = sum scores
       

scoreForPos :: Pos -> Board -> Int
scoreForPos pos board = materialValue (getB pos board) +
                        pieceSquareTable (getB pos board) !! (rowOf pos) !! (colOf pos)

materialValue :: Square -> Int
materialValue (Piece White Pawn) = 100
materialValue (Piece White Knight) = 320
materialValue (Piece White Bishop) = 330
materialValue (Piece White Rook) = 500
materialValue (Piece White Queen) = 900
materialValue (Piece White King) = 200000
materialValue (Piece Black p) = -(materialValue $ Piece White p)
materialValue Empty = 0

pieceSquareTable :: Square -> [[Int]]
pieceSquareTable (Piece White Pawn) =   [[ u,  u,  u,  u,  u,  u,  u,  u],
                                         [50, 50, 50, 50, 50, 50, 50, 50],
                                         [10, 10, 20, 30, 30, 20, 10, 10],
                                         [ 5,  5, 10, 25, 25, 10,  5,  5],
                                         [ 0,  0,  0, 20, 20,  0,  0,  0],
                                         [ 5, -5,-10,  0,  0,-10, -5,  5],
                                         [ 5, 10, 10,-20,-20, 10, 10,  5],
                                         [ u,  u,  u,  u,  u,  u,  u,  u]]
pieceSquareTable (Piece White Knight) = [[-50,-40,-30,-30,-30,-30,-40,-50],
                                         [-40,-20,  0,  0,  0,  0,-20,-40],
                                         [-30,  0, 10, 15, 15, 10,  0,-30],
                                         [-30,  5, 15, 20, 20, 15,  5,-30],
                                         [-30,  0, 15, 20, 20, 15,  0,-30],
                                         [-30,  5, 10, 15, 15, 10,  5,-30],
                                         [-40,-20,  0,  5,  5,  0,-20,-40],
                                         [-50,-40,-30,-30,-30,-30,-40,-50]]
pieceSquareTable (Piece White Bishop) = [[-20,-10,-10,-10,-10,-10,-10,-20],
                                         [-10,  0,  0,  0,  0,  0,  0,-10],
                                         [-10,  0,  5, 10, 10,  5,  0,-10],
                                         [-10,  5,  5, 10, 10,  5,  5,-10],
                                         [-10,  0, 10, 10, 10, 10,  0,-10],
                                         [-10, 10, 10, 10, 10, 10, 10,-10],
                                         [-10,  5,  0,  0,  0,  0,  5,-10],
                                         [-20,-10,-10,-10,-10,-10,-10,-20]]
pieceSquareTable (Piece White Rook) =   [[ 0,  0,  0,  0,  0,  0,  0,  0],
                                         [ 5, 10, 10, 10, 10, 10, 10,  5],
                                         [-5,  0,  0,  0,  0,  0,  0, -5],
                                         [-5,  0,  0,  0,  0,  0,  0, -5],
                                         [-5,  0,  0,  0,  0,  0,  0, -5],
                                         [-5,  0,  0,  0,  0,  0,  0, -5],
                                         [-5,  0,  0,  0,  0,  0,  0, -5],
                                         [ 0,  0,  0,  5,  5,  0,  0,  0]]
pieceSquareTable (Piece White Queen) =   [[-20,-10,-10, -5, -5,-10,-10,-20],
                                         [-10,  0,  0,  0,  0,  0,  0,-10],
                                         [-10,  0,  5,  5,  5,  5,  0,-10],
                                         [ -5,  0,  5,  5,  5,  5,  0, -5],
                                         [  0,  0,  5,  5,  5,  5,  0, -5],
                                         [-10,  5,  5,  5,  5,  5,  0,-10],
                                         [-10,  0,  5,  0,  0,  0,  0,-10],
                                         [-20,-10,-10, -5, -5,-10,-10,-20]]
pieceSquareTable (Piece White King) =   [[-30,-40,-40,-50,-50,-40,-40,-30],
                                         [-30,-40,-40,-50,-50,-40,-40,-30],
                                         [-30,-40,-40,-50,-50,-40,-40,-30],
                                         [-30,-40,-40,-50,-50,-40,-40,-30],
                                         [-20,-30,-30,-40,-40,-30,-30,-20],
                                         [-10,-20,-20,-20,-20,-20,-20,-10],
                                         [ 20, 20,  0,  0,  0,  0, 20, 20],
                                         [ 20, 30, 10,  0,  0, 10, 30, 20]]
pieceSquareTable (Piece Black p) = map (map (* (-1))) $ reverse $
                                   pieceSquareTable $ Piece White p
pieceSquareTable Empty = replicate 8 $ replicate 8 0
    
u = undefined                                   

-- Numerical values from
-- https://www.chessprogramming.org/Simplified_Evaluation_Function
