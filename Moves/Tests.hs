
{-# LANGUAGE TemplateHaskell #-} 

module Moves.Tests where

import Test.QuickCheck
import Data.List

import Board
import Moves


--------------------------------------------------------------------------------
-- Fixed boards
--------------------------------------------------------------------------------

prop_fixedBoard1 :: Property
prop_fixedBoard1 = verifyMoves Black board moves
  where
    board = read  "  0 1 2 3 4 5 6 7  \n\
                  \0 ♟         ♚   ♝ 0\n\
                  \1   ♞           ♝ 1\n\
                  \2   ♔     ♝       2\n\
                  \3     ♜   ♙     ♙ 3\n\
                  \4     ♛   ♖ ♟ ♗ ♚ 4\n\
                  \5     ♖       ♗ ♜ 5\n\
                  \6       ♘ ♞ ♟ ♝ ♟ 6\n\
                  \7   ♔   ♜   ♞ ♖ ♝ 7\n\
                  \  0 1 2 3 4 5 6 7"
    moves = -- King at (0,5)
            (movesFrom (0,5) [(0,4), (0,6), (1,4), (1,5), (1,6)]) ++

            -- King at (4,7)
            (movesFrom (4,7) [(3,7), (3,6), (4,6), (5,6)]) ++

            -- Queen at (4,2)
            (movesFrom (4,2) [(4,3), (4,4), (5,2), (5,1), (6,0), (4,1), (4,0),
                              (3,1), (2,0), (3,3), (5,3)]) ++

            -- Bishop at (0,7)
            (movesFrom (0,7) [(1,6), (2,5), (3,4)]) ++

            -- Bishop at (1,7)
            (movesFrom (1,7) [(2,6), (3,5), (4,4), (0,6)]) ++

            -- Bishop at (2,4)
            (movesFrom (2,4) [(1,5), (0,6), (3,5), (4,6), (3,3), (1,3),
                              (0,2)]) ++

            -- Bishop at (6,6)
            (movesFrom (6,6) [(5,5), (4,4)]) ++

            -- Pawn at (0,0)
            (movesFrom (0,0) [(1,0)]) ++

            -- Pawn at (6,5)
            (movesFrom (6,5) [(7,6)]) ++

            -- Pawn at (6,7)
            (movesFrom (6,7) [(7,6)]) ++

            -- Pawn at (4,5)
            (movesFrom (4,5) [(5,5), (5,6)]) ++

            -- Rook at (7,3)
            (movesFrom (7,3) [(7,4), (7,2), (7,1), (6,3)]) ++

            -- Rook at (5,7)
            (movesFrom (5,7) [(5,6)]) ++

            -- Rook at (3,2)
            (movesFrom (3,2) [(2,2), (1,2), (0,2), (3,3), (3,4), (3,1),
                              (3,0)]) ++

            -- Knight at (1,1)
            (movesFrom (1,1) [(0,3), (2,3), (3,0)]) ++

            -- Knight at (7,5)
            (movesFrom (7,5) [(5,6), (5,4), (6,3)]) ++

            -- Knight at (6,4)
            (movesFrom (6,4) [(4,3), (5,6), (7,6), (7,2), (5,2)])

prop_fixedBoard2 :: Property
prop_fixedBoard2 = verifyMoves White board moves
  where
    board = read  "  0 1 2 3 4 5 6 7  \n\
                  \0                 0\n\
                  \1                 1\n\
                  \2                 2\n\
                  \3                 3\n\
                  \4                 4\n\
                  \5         ♚       5\n\
                  \6       ♙         6\n\
                  \7 ♔               7\n\
                  \  0 1 2 3 4 5 6 7"
    moves = -- King at (7,0)
            (movesFrom (7,0) [(6,0), (7,1), (6,1)]) ++

            -- Pawn at (6,3)
            (movesFrom (6,3) [(5,3), (4,3), (5,4)])

verifyMoves :: Color -> Board -> [((Int,Int),(Int,Int))] -> Property
verifyMoves color board expectedMoves =
  counterexample errorString verificationResult
  where
    expectedMoves'     = sort expectedMoves
    actualMoves        = sort $ movesForColor color board
    verificationResult = expectedMoves' == actualMoves
    actualMissing      = expectedMoves' \\ actualMoves
    actualExtra        = actualMoves    \\ expectedMoves'
    errorString        = show board ++ "\n" ++
                         "Expected moves: " ++ show expectedMoves' ++ "\n" ++
                         "Actual moves:   " ++ show actualMoves ++ "\n" ++
                         "Actual is missing: " ++ show actualMissing ++ "\n" ++
                         "Actual has these extra: " ++ show actualExtra

movesFrom :: (Int,Int) -> [(Int,Int)] -> [((Int,Int),(Int,Int))]
movesFrom start ends = map (\end -> (start, end)) ends


--------------------------------------------------------------------------------
-- Subsets
--------------------------------------------------------------------------------

prop_rookMovesAreSubsetOfQueenMoves :: Board -> (Int,Int) -> Bool
prop_rookMovesAreSubsetOfQueenMoves = (Rook `movesAreSubsetOfMoves` Queen)

prop_bishopMovesAreSubsetOfQueenMoves :: Board -> (Int,Int) -> Bool
prop_bishopMovesAreSubsetOfQueenMoves = (Bishop `movesAreSubsetOfMoves` Queen)

prop_kingMovesAreSubsetOfQueenMoves :: Board -> (Int,Int) -> Bool
prop_kingMovesAreSubsetOfQueenMoves = (King `movesAreSubsetOfMoves` Queen)

prop_pawnMovesAreSubsetOfQueenMoves :: Board -> (Int,Int) -> Bool
prop_pawnMovesAreSubsetOfQueenMoves = (Pawn `movesAreSubsetOfMoves` Queen)

movesAreSubsetOfMoves :: Kind -> Kind -> (Board -> (Int,Int) -> Bool)
movesAreSubsetOfMoves kind1 kind2 board pos = movesKind1 `isSubsetOf`
                                              movesKind2
  where
    pos' = withinizePos pos

    boardKind1 = set pos' (Piece Black kind1) board
    boardKind2 = set pos' (Piece Black kind2) board

    movesKind1 = movesForColor Black boardKind1
    movesKind2 = movesForColor Black boardKind2

--------------------------------------------------------------------------------
-- All pieces (-)
--------------------------------------------------------------------------------

-- No non moves

prop_destinationIsWithinBoard :: Board -> Bool
prop_destinationIsWithinBoard board = all pred moves
  where
    moves = movesForColor Black board
    pred (_start,(row,col)) = 0 <= row && row < 8 && 0 <= col && col < 8

prop_destinationIsNotSameColor :: Board -> Bool
prop_destinationIsNotSameColor board = all pred moves
  where
    moves              = movesForColor Black board
    pred (_start,dest) = not (isColor Black (get dest board))

prop_startIsSameColor :: Board -> Bool
prop_startIsSameColor board = all pred moves
  where
    moves              = movesForColor Black board
    pred (start,_dest) = isColor Black (get start board)

prop_blackAndWhiteGiveSameMoves :: Board -> Bool
prop_blackAndWhiteGiveSameMoves board = blackMoves `eqMoves` mirroredWhiteMoves
  where
    blackMoves         = movesForColor Black board
    swappedColors      = swapColors board
    mirroredBoard      = mirrorBoard swappedColors
    whiteMoves         = movesForColor White mirroredBoard
    mirroredWhiteMoves = map mirrorMove whiteMoves

swapColors :: Board -> Board
swapColors board = foldl swapColorPos
                         board
                         [(row,col) | row <- [0..7], col <- [0..7]]

swapColorPos :: Board -> (Int,Int) -> Board
swapColorPos board pos = set pos newAtPos board
  where
    atPos    = get pos board
    newAtPos = swapColor atPos

swapColor :: Square -> Square
swapColor Empty = Empty
swapColor (Piece White kind) = Piece Black kind
swapColor (Piece Black kind) = Piece White kind

mirrorBoard :: Board -> Board
mirrorBoard board = foldl mirrorPos
                          board
                          [(row,col) | row <- [0..3], col <- [0..7]]

mirrorPos :: Board -> (Int,Int) -> Board
mirrorPos board pos@(row,col) = set pos atMirroredPos $
                                set mirroredPos atPos $ board
  where
    mirroredRow   = 7-row
    mirroredPos   = (mirroredRow,col)
    atPos         = get pos board
    atMirroredPos = get mirroredPos board

mirrorMove :: ((Int,Int),(Int,Int)) -> ((Int,Int),(Int,Int))
mirrorMove ((rowS,colS),(rowD,colD)) = ((rowS',colS),(rowD',colD))
  where
    rowS' = 7-rowS
    rowD' = 7-rowD

eqMoves :: [((Int,Int),(Int,Int))] -> [((Int,Int),(Int,Int))] -> Bool
eqMoves moves1 moves2 = sort moves1 == sort moves2


--------------------------------------------------------------------------------
-- King, Queen, Rook and Bishop (-)
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Moves in valid direction
--------------------------------------------------------------------------------

prop_kingOnlyMovesStraight :: Board -> (Int,Int) -> Bool
prop_kingOnlyMovesStraight = pieceOnlyMoves King isMoveStraight

prop_kingOnlyMovesOneStep :: Board -> (Int,Int) -> Bool
prop_kingOnlyMovesOneStep = pieceOnlyMoves King isMoveOneStep

prop_queenOnlyMovesStraight :: Board -> (Int,Int) -> Bool
prop_queenOnlyMovesStraight = pieceOnlyMoves Queen isMoveStraight

prop_rookOnlyMovesHorizontallyOrVertically :: Board -> (Int,Int) -> Bool
prop_rookOnlyMovesHorizontallyOrVertically =
  pieceOnlyMoves Rook isMoveHorizontalOrVertical

prop_bishopOnlyMovesDiagonally :: Board -> (Int,Int) -> Bool
prop_bishopOnlyMovesDiagonally = pieceOnlyMoves Bishop isMoveDiagonal

pieceOnlyMoves :: Kind ->
                  (((Int,Int),(Int,Int)) -> Bool) ->
                  Board ->
                  (Int,Int) ->
                  Bool
pieceOnlyMoves kind movePred board pos = all movePred moves
  where
    (_board', _pos', moves) = placePieceAndGetMoves board pos kind

isMoveStraight :: ((Int,Int),(Int,Int)) -> Bool
isMoveStraight move = isMoveHorizontalOrVertical move || isMoveDiagonal move

isMoveHorizontalOrVertical :: ((Int,Int),(Int,Int)) -> Bool
isMoveHorizontalOrVertical ((rowS,colS),(rowD,colD)) = rowS-rowD == 0 ||
                                                       colS-colD == 0

isMoveDiagonal :: ((Int,Int),(Int,Int)) -> Bool
isMoveDiagonal ((rowS,colS),(rowD,colD)) = abs (rowS-rowD) == abs (colS-colD)

--------------------------------------------------------------------------------
-- Empty between start and dest
--------------------------------------------------------------------------------

prop_queenEmptyBetweenStartAndDest :: Board -> (Int,Int) -> Bool
prop_queenEmptyBetweenStartAndDest = emptyBetweenStartAndDestForKind Queen

prop_rookEmptyBetweenStartAndDest :: Board -> (Int,Int) -> Bool
prop_rookEmptyBetweenStartAndDest = emptyBetweenStartAndDestForKind Rook

prop_bishopEmptyBetweenStartAndDest :: Board -> (Int,Int) -> Bool
prop_bishopEmptyBetweenStartAndDest = emptyBetweenStartAndDestForKind Bishop

emptyBetweenStartAndDestForKind :: Kind -> Board -> (Int,Int) -> Bool
emptyBetweenStartAndDestForKind kind board pos =
  all (emptyBetweenStartAndDest board') moves
  where
    (board', _pos', moves) = placePieceAndGetMoves board pos kind


--------------------------------------------------------------------------------
-- King, Queen, Rook and Bishop (+)
--------------------------------------------------------------------------------

prop_kingMovesIfCan :: Board -> (Int,Int) -> Int -> Int -> Property
prop_kingMovesIfCan = let maxLength = 1
                      in  movesIfCan getDirKingQueen King maxLength

prop_queenMovesIfCan :: Board -> (Int,Int) -> Int -> Int -> Property
prop_queenMovesIfCan = let maxLength = 7
                       in  movesIfCan getDirKingQueen Queen maxLength

prop_rookMovesIfCan :: Board -> (Int,Int) -> Int -> Int -> Property
prop_rookMovesIfCan = let maxLength = 7
                      in  movesIfCan getDirRook Rook maxLength

prop_bishopMovesIfCan :: Board -> (Int,Int) -> Int -> Int -> Property
prop_bishopMovesIfCan = let maxLength = 7
                        in  movesIfCan getDirBishop Bishop maxLength

getDirKingQueen :: Int -> (Int,Int)
getDirKingQueen dir = case dir `mod` 8 of
                        0 -> (0,1)   -- Right
                        1 -> (1,0)   -- Down
                        2 -> (0,-1)  -- Left
                        3 -> (-1,0)  -- Up
                        4 -> (1,1)   -- Down right
                        5 -> (1,-1)  -- Down left
                        6 -> (-1,1)  -- Up right
                        7 -> (-1,-1) -- Up left

getDirRook :: Int -> (Int,Int)
getDirRook dir = case dir `mod` 4 of
                    0 -> (0,1)  -- Right
                    1 -> (1,0)  -- Down
                    2 -> (0,-1) -- Left
                    3 -> (-1,0) -- Up

getDirBishop :: Int -> (Int,Int)
getDirBishop dir = case dir `mod` 4 of
                      0 -> (1,1)   -- Down right
                      1 -> (1,-1)  -- Down left
                      2 -> (-1,1)  -- Up right
                      3 -> (-1,-1) -- Up left

movesIfCan :: (Int -> (Int,Int)) ->
              Kind ->
              Int ->
              Board ->
              (Int,Int) ->
              Int ->
              Int ->
              Property
movesIfCan getDir kind maxLength board start dir length =
  isWithinBoard dest &&
  not (isColor Black atDest) &&
  emptyBetweenStartAndDest board' move ==>
  move `elem` moves
    where
      (board', start', moves) = placePieceAndGetMoves board start kind
      dir'                    = getDir dir
      length'                 = length `mod` maxLength + 1
      diff                    = dir' `tupleMul` length'
      dest                    = start' `tupleAdd` diff
      move                    = (start',dest)
      atDest                  = get dest board'


--------------------------------------------------------------------------------
-- Pawn (+ and -)
--------------------------------------------------------------------------------

prop_pawnSingleForward :: Board -> (Int,Int) -> Property
prop_pawnSingleForward board pos = isWithinBoard dest ==>
                                   (move `elem` moves) ==
                                   isEmpty atDest
  where
    (board', start, moves) = placePieceAndGetMoves board pos Pawn
    dest                   = start `tupleAdd` (1,0)
    atDest                 = get dest board'
    move                   = (start,dest)

prop_pawnDoubleForward :: Board -> (Int,Int) -> Property
prop_pawnDoubleForward board pos = isWithinBoard dest ==>
                                   (move `elem` moves) ==
                                   (isEmpty atBetween && isEmpty atDest
                                    && startRow == 1)
  where
    (board', start, moves) = placePieceAndGetMoves board pos Pawn
    (startRow, _startCol)  = start
    between                = start `tupleAdd` (1,0)
    dest                   = start `tupleAdd` (2,0)
    atBetween              = get between board'
    atDest                 = get dest board'
    move                   = (start,dest)

prop_pawnMovesRight :: Board -> (Int,Int) -> Property
prop_pawnMovesRight board pos = isWithinBoard dest ==>
                                (move `elem` moves) ==
                                isOtherColor Black atDest
  where
    (board', start, moves) = placePieceAndGetMoves board pos Pawn
    dest                   = start `tupleAdd` (1,1)
    atDest                 = get dest board'
    move                   = (start,dest)

prop_pawnMovesLeft :: Board -> (Int,Int) -> Property
prop_pawnMovesLeft board pos = isWithinBoard dest ==>
                               (move `elem` moves) ==
                               isOtherColor Black atDest
  where
    (board', start, moves) = placePieceAndGetMoves board pos Pawn
    dest                   = start `tupleAdd` (1,-1)
    atDest                 = get dest board'
    move                   = (start,dest)

prop_pawnNoOtherMove :: Board -> (Int,Int) -> Property
prop_pawnNoOtherMove board pos = counterexample errorString 
                                                (moves `isSubsetOf` legalMoves)
  where
    (board', start, moves) = placePieceAndGetMoves board pos Pawn
    singleForward          = (start, start `tupleAdd` (1,0))
    doubleForward          = (start, start `tupleAdd` (2,0))
    right                  = (start, start `tupleAdd` (1,1))
    left                   = (start, start `tupleAdd` (1,-1))
    legalMoves             = [singleForward,doubleForward,right,left]
    errorString            = show board' ++ "\n" ++ show moves


--------------------------------------------------------------------------------
-- Knight (-)
--------------------------------------------------------------------------------

prop_knightOnlyMovesLShaped :: Board -> (Int,Int) -> Bool
prop_knightOnlyMovesLShaped board pos = all isMoveLShaped moves
  where
    (_board', _start, moves) = placePieceAndGetMoves board pos Knight

isMoveLShaped :: ((Int,Int),(Int,Int)) -> Bool
isMoveLShaped ((rowS,colS),(rowD,colD)) = rowDiff `elem` [1,2] &&
                                          colDiff `elem` [1,2] &&
                                          rowDiff+colDiff == 3
  where
    rowDiff = abs $ rowD-rowS
    colDiff = abs $ colD-colS


--------------------------------------------------------------------------------
-- Knight (+)
--------------------------------------------------------------------------------

prop_knightMovesIfCan :: Board -> (Int,Int) -> Int -> Int -> Property
prop_knightMovesIfCan board pos rowDiff colDiff = isWithinBoard dest && 
                                                  isMoveLShaped move && 
                                                  not (isColor Black atDest) ==>
                                                  move `elem` moves
  where
    (board', start, moves) = placePieceAndGetMoves board pos Knight
    rowDiff'               = fixDiff rowDiff
    colDiff'               = fixDiff colDiff
    diff                   = (rowDiff',colDiff')
    dest                   = start `tupleAdd` diff
    atDest                 = get dest board'
    move                   = (start,dest)

    fixDiff diff = case diff `mod` 4 of
                      0 -> -2
                      1 -> -1
                      2 -> 1
                      3 -> 2


--------------------------------------------------------------------------------
-- General helper functions
--------------------------------------------------------------------------------

placePieceAndGetMoves :: Board ->
                         (Int,Int) ->
                         Kind ->
                         (Board, (Int,Int), [((Int,Int),(Int,Int))])
placePieceAndGetMoves board pos kind = (board', pos', movesPlaced)
  where
    pos'        = withinizePos pos
    board'      = set pos' (Piece Black kind) board
    moves       = movesForColor Black board'
    movesPlaced = filter (isMoveFrom pos') moves

emptyBetweenStartAndDest :: Board -> ((Int,Int),(Int,Int)) -> Bool
emptyBetweenStartAndDest board (start,dest) =
  and [isEmpty (get p board) | p <- between]
  where
    diff    = dest `tupleSub` start
    dir     = tupleSignum diff
    length  = tupleMaxAbs diff
    between = map ((start `tupleAdd`) . (dir `tupleMul`)) [1..length-1]

--------------------------------------------------------------------------------
-- Small/misc helper functions
--------------------------------------------------------------------------------

isSubsetOf :: (Eq a) => [a] -> [a] -> Bool
xs `isSubsetOf` ys = null $ xs \\ ys

tupleAdd :: (Int,Int) -> (Int,Int) -> (Int,Int)
tupleAdd (a,b) (c,d) = (a+c,b+d)

tupleSub :: (Int,Int) -> (Int,Int) -> (Int,Int)
tupleSub (a,b) (c,d) = (a-c,b-d)

tupleMul :: (Int,Int) -> Int -> (Int,Int)
tupleMul (a,b) c = (a*c,b*c)

tupleSignum :: (Int,Int) -> (Int,Int)
tupleSignum (a,b) = (signum a,signum b)

tupleMaxAbs :: (Int,Int) -> Int
tupleMaxAbs (a,b) = max (abs a) (abs b)

withinizePos :: (Int,Int) -> (Int,Int)
withinizePos (row,col) = (row `mod` 8, col `mod` 8)

isWithinBoard :: (Int,Int) -> Bool
isWithinBoard (row,col) = 0 <= row && row < 8 && 0 <= col && col < 8

isMoveOneStep :: ((Int,Int),(Int,Int)) -> Bool
isMoveOneStep (start,dest) = tupleMaxAbs (dest `tupleSub` start) <= 1

isMoveFrom :: (Int,Int) -> ((Int,Int),(Int,Int)) -> Bool
isMoveFrom pos (start,_dest) = pos == start



return []
runTests = $quickCheckAll
