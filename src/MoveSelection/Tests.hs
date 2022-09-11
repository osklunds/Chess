{-# LANGUAGE TemplateHaskell #-}

module MoveSelection.Tests where

import Test.QuickCheck

import Board
import MoveSelection
import Moves

--------------------------------------------------------------------------------
-- Fixed
--------------------------------------------------------------------------------
prop_capture :: Bool
prop_capture = all verifyCapturesPiece [Queen, Rook, Bishop, Knight, Pawn]

verifyCapturesPiece :: Kind -> Bool
verifyCapturesPiece kind = verifyMakesMove ((5, 5), pos) board'
  where
    pos = (6, 4)
    piece = Piece White kind
    board =
        read
            "  0 1 2 3 4 5 6 7  \n\
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

verifyMakesMove :: ((Int, Int), (Int, Int)) -> Board -> Bool
verifyMakesMove move board = all f [1 .. 6]
  where
    f depth = makeMove depth board == move

prop_escape :: Bool
prop_escape = and [verifyEscapesFromThreat d k | d <- depths, k <- kinds]
  where
    depths = [2 .. 4]
    kinds = [Queen, Rook, Bishop, Knight, Pawn]

verifyEscapesFromThreat :: Int -> Kind -> Bool
verifyEscapesFromThreat depth kind =
    all (\pos -> isEmpty $ getB pos board'') threatenedPosList
  where
    curPos = (6, 4)
    threatenedPosList = [curPos, (6, 2), (0, 0), (0, 1), (1, 1), (2, 1), (2, 0)]
    piece = Piece Black kind
    board =
        read
            "  0 1 2 3 4 5 6 7  \n\
                    \0               ♚ 0\n\
                    \1 ♔               1\n\
                    \2                 2\n\
                    \3                 3\n\
                    \4                 4\n\
                    \5                 5\n\
                    \6        [ ]      6\n\
                    \7       ♙         7\n\
                    \  0 1 2 3 4 5 6 7"
    board' = setB curPos piece board
    move = makeMove depth board'
    board'' = applyMove move board'

prop_checkmate :: Bool
prop_checkmate = and [makeMove d board `elem` moves | d <- [1 .. 7]]
  where
    moves = [((7, 1), (7, 0)), ((6, 1), (6, 0))]
    -- The board looks like this to reduce the number of moves to speed up
    board =
        read
            "  0 1 2 3 4 5 6 7  \n\
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
    and [makeMove d board `elem` moves | d <- [1 .. 5]]
  where
    moves = [((3, 0), (1, 2)), ((3, 0), (5, 2))]
    -- The board looks like this to reduce the number of moves to speed up
    board =
        read
            "  0 1 2 3 4 5 6 7  \n\
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
    and [isEmpty $ getB (4, 4) (nextBoard d) | d <- [1 .. 5]]
  where
    board =
        read
            "  0 1 2 3 4 5 6 7  \n\
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
prop_doStalemateIfLosing =
    and [makeMove d board == ((6, 5), (7, 6)) | d <- [2 .. 5]]
  where
    board =
        read
            "  0 1 2 3 4 5 6 7  \n\
                  \0       ♚         0\n\
                  \1                 1\n\
                  \2 ♙   ♕ ♙         2\n\
                  \3   ♙             3\n\
                  \4                 4\n\
                  \5               ♙ 5\n\
                  \6           ♛ ♙   6\n\
                  \7               ♔ 7\n\
                  \  0 1 2 3 4 5 6 7"

-- The above board comes from
-- https://www.chessgames.com/perl/chessgame?gid=1255706
-- James Adams Congdon vs Eugene Delmar
-- Right now, Black is in a losing position. There's not much hope for Black
-- to win, but there's a possibility to create a draw and thus at least avoid
-- losing.
-- TODO: Test move that needs two steps ahead thinking
--------------------------------------------------------------------------------
-- Arbitrary
--------------------------------------------------------------------------------
prop_legalMove :: Int -> Board -> Property
prop_legalMove d board =
    not (null legalMoves) ==> makeMove depth board `elem` legalMoves
  where
    legalMoves = movesForColor Black board
    depth = 1 + d `mod` 4

-- TODO: escape from check
--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------
makeMove :: Int -> Board -> ((Int, Int), (Int, Int))
makeMove depth board = moveColor depth Black board

return []

runTests = $quickCheckAll
