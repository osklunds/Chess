
{-# LANGUAGE TemplateHaskell #-}

module MoveSelection.Tests where

import Test.QuickCheck

import Board
import Moves.CheckAware
import MoveSelection


--------------------------------------------------------------------------------
-- Fixed
--------------------------------------------------------------------------------

prop_capture :: Bool
prop_capture = all verifyCapturesPiece
                   [Queen, Rook, Bishop, Knight, Pawn]

verifyCapturesPiece :: Kind -> Bool
verifyCapturesPiece kind = verifyMakesMove ((5,5),pos) board'
  where
    pos    = (6,4)
    piece  = Piece White kind

    board  = read  "  0 1 2 3 4 5 6 7  \n\
                   \0                 0\n\
                   \1 ♔               1\n\
                   \2                 2\n\
                   \3                 3\n\
                   \4                 4\n\
                   \5           ♟ ♚   5\n\
                   \6        [ ]      6\n\
                   \7                 7\n\
                   \  0 1 2 3 4 5 6 7"
    board' = setB pos piece board

verifyMakesMove :: ((Int,Int),(Int,Int)) -> Board -> Bool
verifyMakesMove move board = all f [2..5]
  where
    f depth = makeMove depth board == move

prop_escape :: Bool
prop_escape = and [verifyEscapesFromThreat d k | d <- depths, k <- kinds]
  where
    depths = [2..5]
    kinds  = [Queen, Rook, Bishop, Knight, Pawn]

verifyEscapesFromThreat :: Int -> Kind -> Bool
verifyEscapesFromThreat depth kind = all (\pos -> isEmpty $ getB pos board'') 
                                         threatenedPosList
  where
    curPos            = (6,4)
    threatenedPosList = [curPos,(6,2),(0,0),(0,1),(1,1),(2,1),(2,0)]
    piece             = Piece Black kind

    board   = read  "  0 1 2 3 4 5 6 7  \n\
                    \0               ♚ 0\n\
                    \1 ♔               1\n\
                    \2                 2\n\
                    \3                 3\n\
                    \4                 4\n\
                    \5                 5\n\
                    \6        [ ]      6\n\
                    \7       ♙         7\n\
                    \  0 1 2 3 4 5 6 7"
    board'  = setB curPos piece board
    move    = makeMove depth board'
    board'' = applyMove move board'

-- TODO: checkmate by mvoing away other piece
-- TODO: escape from check

prop_checkmate :: Bool
prop_checkmate = and [makeMove d board `elem` moves | d <- [5..5]]
  where
    moves = [((0,4),(0,3)), ((4,2),(3,3))]
board     = read  "  0 1 2 3 4 5 6 7  \n\
                  \0 ♟       ♜     ♝ 0\n\
                  \1     ♞     ♛   ♝ 1\n\
                  \2       ♔ ♝       2\n\
                  \3     ♜   ♙     ♙ 3\n\
                  \4     ♛   ♖ ♟ ♗ ♚ 4\n\
                  \5     ♖         ♜ 5\n\
                  \6       ♘ ♞ ♟ ♝ ♟ 6\n\
                  \7       ♜   ♞ ♖ ♝ 7\n\
                  \  0 1 2 3 4 5 6 7"

-- TODO: The above fails. MiniMax doesn't realize it's checkmate because
-- opponent has no moves, so evaluates current, but current should be
-- maxBound because checkmate.

-- TODO: Above fails because can make irrelevant move and
-- later do the checkmate move
-- Score needs to consider depth, include depth in genSts




--------------------------------------------------------------------------------
-- Arbitrary
--------------------------------------------------------------------------------

prop_legalMove :: Int -> Board -> Bool
prop_legalMove d board = makeMove d' board `elem` movesForColor Black board
  where
    d' = 2 + d `mod` 3


--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

makeMove :: Int -> Board -> ((Int,Int),(Int,Int))
makeMove depth board = moveColor depth Black board





return []
runTests = $quickCheckAll