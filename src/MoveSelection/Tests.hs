
{-# LANGUAGE TemplateHaskell #-}

module MoveSelection.Tests where

import Test.QuickCheck

import Board
import Moves
import MoveSelection


--------------------------------------------------------------------------------
-- Fixed
--------------------------------------------------------------------------------

prop_capture :: Bool
prop_capture = all verifyCapturesPiece
                   [Queen, Rook, Bishop, Knight, Pawn]

verifyCapturesPiece :: Kind -> Bool
verifyCapturesPiece kind = verifyMakesMove move board'
  where
    src    = Pos 5 5
    dst    = Pos 6 4
    move   = NormalMove src dst
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
    board' = setB dst piece board

verifyMakesMove :: Move -> Board -> Bool
verifyMakesMove move board = all f [1..5]
  where
    f depth = makeMove depth board == move

prop_escape :: Bool
prop_escape = and [verifyEscapesFromThreat d k | d <- depths, k <- kinds]
  where
    depths = [2..4]
    kinds  = [Queen, Rook, Bishop, Knight, Pawn]

verifyEscapesFromThreat :: Int -> Kind -> Bool
verifyEscapesFromThreat depth kind = all (\pos -> isEmpty $ getB pos board'') 
                                         threatenedPosList
  where
    curPos            = Pos 6 4
    threatenedPosList = [curPos, Pos 6 2, Pos 0 0, Pos 0 1, Pos 1 1,
                                 Pos 2 1, Pos 2 0]
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

prop_checkmate :: Bool
prop_checkmate = and [makeMove d board `elem` moves | d <- [1..5]]
  where
    moves = [NormalMove (Pos 7 1) (Pos 7 0), NormalMove (Pos 6 1) (Pos 6 0)]
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

prop_checkmateByMovingAwayPiece :: Bool
prop_checkmateByMovingAwayPiece =
  and [makeMove d board `elem` moves | d <- [1..5]]
  where
    moves = [NormalMove (Pos 3 0) (Pos 1 2), NormalMove (Pos 3 0) (Pos 5 2)]
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

prop_escapeFromCheckEvenIfCanCheckmate :: Bool
prop_escapeFromCheckEvenIfCanCheckmate =
  and [isEmpty $ getB (Pos 4 4) (nextBoard d) | d <- [1..5]]
  where
    board = read  "  0 1 2 3 4 5 6 7  \n\
                  \0 ♔               0\n\
                  \1                 1\n\
                  \2           ♘     2\n\
                  \3                 3\n\
                  \4         ♚       4\n\
                  \5                 5\n\
                  \6     ♜           6\n\
                  \7   ♜             7\n\
                  \  0 1 2 3 4 5 6 7"
    nextBoard d = applyMove (makeMove d board) board

prop_doStalemateIfLosing :: Bool
prop_doStalemateIfLosing = and [makeMove d board == expMove | d <- [2..5]]
  where
    board = read  "  0 1 2 3 4 5 6 7  \n\
                  \0       ♚         0\n\
                  \1                 1\n\
                  \2 ♙   ♕ ♙         2\n\
                  \3   ♙             3\n\
                  \4                 4\n\
                  \5               ♙ 5\n\
                  \6           ♛ ♙   6\n\
                  \7               ♔ 7\n\
                  \  0 1 2 3 4 5 6 7"
    expMove = NormalMove (Pos 6 5) (Pos 7 6)
-- The above board comes from
-- https://www.chessgames.com/perl/chessgame?gid=1255706
-- James Adams Congdon vs Eugene Delmar
-- Right now, Black is in a losing position. There's not much hope for Black
-- to win, but there's a possibility to create a draw and thus at least avoid
-- losing.

-- TODO: Test move that needs two steps ahead thinking

-- TODO: Promote to non-queen

--------------------------------------------------------------------------------
-- Arbitrary
--------------------------------------------------------------------------------

prop_legalMove :: Int -> Board -> Property
prop_legalMove d board = not (null legalMoves) ==>
                         makeMove depth board `elem` legalMoves
  where
    legalMoves = movesF Black board
    depth      = 1 + d `mod` 3

prop_performance :: Board -> Property
prop_performance board = not (null legalMoves) ==>
                         makeMove 1 board `elem` legalMoves
  where
    legalMoves = movesF Black board

-- TODO: escape from check

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

makeMove :: Int -> Board -> Move
makeMove depth board = moveColor depth Black board


return []
runTests = $quickCheckAll
