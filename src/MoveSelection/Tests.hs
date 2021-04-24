
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
prop_checkmate = and [makeMove d board `elem` moves | d <- [2..5]]
  where
    moves = [((7,1),(7,0)), ((6,1),(6,0))]
    -- The board looks like this to reduce the number of moves to speed up
    board = read  "  0 1 2 3 4 5 6 7  \n\
                  \0 ♔   ♟           0\n\
                  \1     ♟           1\n\
                  \2     ♟           2\n\
                  \3     ♟           3\n\
                  \4     ♟         ♚ 4\n\
                  \5     ♟           5\n\
                  \6   ♜ ♟           6\n\
                  \7   ♜ ♟           7\n\
                  \  0 1 2 3 4 5 6 7"

-- TODO: Test performance with fold in score
-- TODO: Test more depths
-- TODO: Test move that needs two steps ahead thinking

prop_checkmateByMovingAwayPiece :: Bool
prop_checkmateByMovingAwayPiece =
  and [makeMove d board `elem` moves | d <- [2..5]]
  where
    moves = [((3,0),(1,2)), ((3,0),(5,2))]
    -- The board looks like this to reduce the number of moves to speed up
    board = read  "  0 1 2 3 4 5 6 7  \n\
                  \0 ♔     ♟         0\n\
                  \1       ♟         1\n\
                  \2       ♟         2\n\
                  \3 ♝     ♟         3\n\
                  \4       ♟       ♚ 4\n\
                  \5       ♟         5\n\
                  \6       ♟         6\n\
                  \7 ♜ ♜   ♟         7\n\
                  \  0 1 2 3 4 5 6 7"


--------------------------------------------------------------------------------
-- Arbitrary
--------------------------------------------------------------------------------

prop_legalMove :: Int -> Board -> Property
prop_legalMove d board = not (null legalMoves) ==>
                         makeMove d' board `elem` legalMoves
  where
    legalMoves = movesForColor Black board
    d'         = 2 + d `mod` 3


--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

makeMove :: Int -> Board -> ((Int,Int),(Int,Int))
makeMove depth board = moveColor depth Black board


return []
runTests = $quickCheckAll