
module Score.PieceSquareTable
( allPieceSquareTables
) where

import Data.List

import Types

allPieceSquareTables :: [((Int,Int,Int,Int,Int,Int), (Int,Int,Int,Int,Int,Int), Int)]
allPieceSquareTables = zip3 whitePieceSquareTables
                            blackPieceSquareTables
                            (pieceSquareTable Empty)

whitePieceSquareTables = zip6 (pieceSquareTable (Piece White Pawn))
                              (pieceSquareTable (Piece White Knight))
                              (pieceSquareTable (Piece White Bishop))
                              (pieceSquareTable (Piece White Rook))
                              (pieceSquareTable (Piece White Queen))
                              (pieceSquareTable (Piece White King))

blackPieceSquareTables = zip6 (pieceSquareTable (Piece Black Pawn))
                              (pieceSquareTable (Piece Black Knight))
                              (pieceSquareTable (Piece Black Bishop))
                              (pieceSquareTable (Piece Black Rook))
                              (pieceSquareTable (Piece Black Queen))
                              (pieceSquareTable (Piece Black King))

pieceSquareTable :: Square -> [Int]
pieceSquareTable (Piece White Pawn) =   [ 0,  0,  0,  0,  0,  0,  0,  0,
                                         50, 50, 50, 50, 50, 50, 50, 50,
                                         10, 10, 20, 30, 30, 20, 10, 10,
                                          5,  5, 10, 25, 25, 10,  5,  5,
                                          0,  0,  0, 20, 20,  0,  0,  0,
                                          5, -5,-10,  0,  0,-10, -5,  5,
                                          5, 10, 10,-20,-20, 10, 10,  5,
                                          0,  0,  0,  0,  0,  0,  0,  0]
pieceSquareTable (Piece White Knight) = [-50,-40,-30,-30,-30,-30,-40,-50,
                                         -40,-20,  0,  0,  0,  0,-20,-40,
                                         -30,  0, 10, 15, 15, 10,  0,-30,
                                         -30,  5, 15, 20, 20, 15,  5,-30,
                                         -30,  0, 15, 20, 20, 15,  0,-30,
                                         -30,  5, 10, 15, 15, 10,  5,-30,
                                         -40,-20,  0,  5,  5,  0,-20,-40,
                                         -50,-40,-30,-30,-30,-30,-40,-50]
pieceSquareTable (Piece White Bishop) = [-20,-10,-10,-10,-10,-10,-10,-20,
                                         -10,  0,  0,  0,  0,  0,  0,-10,
                                         -10,  0,  5, 10, 10,  5,  0,-10,
                                         -10,  5,  5, 10, 10,  5,  5,-10,
                                         -10,  0, 10, 10, 10, 10,  0,-10,
                                         -10, 10, 10, 10, 10, 10, 10,-10,
                                         -10,  5,  0,  0,  0,  0,  5,-10,
                                         -20,-10,-10,-10,-10,-10,-10,-20]
pieceSquareTable (Piece White Rook) =   [ 0,  0,  0,  0,  0,  0,  0,  0,
                                          5, 10, 10, 10, 10, 10, 10,  5,
                                         -5,  0,  0,  0,  0,  0,  0, -5,
                                         -5,  0,  0,  0,  0,  0,  0, -5,
                                         -5,  0,  0,  0,  0,  0,  0, -5,
                                         -5,  0,  0,  0,  0,  0,  0, -5,
                                         -5,  0,  0,  0,  0,  0,  0, -5,
                                          0,  0,  0,  5,  5,  0,  0,  0]
pieceSquareTable (Piece White Queen) =   [-20,-10,-10, -5, -5,-10,-10,-20,
                                         -10,  0,  0,  0,  0,  0,  0,-10,
                                         -10,  0,  5,  5,  5,  5,  0,-10,
                                          -5,  0,  5,  5,  5,  5,  0, -5,
                                           0,  0,  5,  5,  5,  5,  0, -5,
                                         -10,  5,  5,  5,  5,  5,  0,-10,
                                         -10,  0,  5,  0,  0,  0,  0,-10,
                                         -20,-10,-10, -5, -5,-10,-10,-20]
pieceSquareTable (Piece White King) =   [-30,-40,-40,-50,-50,-40,-40,-30,
                                         -30,-40,-40,-50,-50,-40,-40,-30,
                                         -30,-40,-40,-50,-50,-40,-40,-30,
                                         -30,-40,-40,-50,-50,-40,-40,-30,
                                         -20,-30,-30,-40,-40,-30,-30,-20,
                                         -10,-20,-20,-20,-20,-20,-20,-10,
                                          20, 20,  0,  0,  0,  0, 20, 20,
                                          20, 30, 10,  0,  0, 10, 30, 20]
pieceSquareTable (Piece Black p) = map negate $ reverse $
                                   pieceSquareTable $ Piece White p
pieceSquareTable Empty = replicate 64 0
    
