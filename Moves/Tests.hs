
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
prop_fixedBoard1 = verifyMoves board moves
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
            (movesFrom (3,2) [(2,2), (1,2), (0,2), (3,3), (3,4), (3,1), (3,0)])

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

-- prop_pawnMovesAreSubsetOfKingMoves :: Board -> (Int,Int) -> Bool
-- prop_pawnMovesAreSubsetOfKingMoves = (Pawn `movesAreSubsetOfMoves` King)

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

prop_destinationIsWithinBoard :: Board -> Bool
prop_destinationIsWithinBoard board = all f moves
  where
    moves = movesForColor Black board
    f (_start,(row,col)) = 0 <= row && row < 8 && 0 <= col && col < 8

prop_destinationIsNotSameColor :: Board -> Bool
prop_destinationIsNotSameColor board = all f moves
  where
    moves           = movesForColor Black board
    f (_start,dest) = not (isColor Black (get dest board))

prop_startIsSameColor :: Board -> Bool
prop_startIsSameColor board = all f moves
  where
    moves           = movesForColor Black board
    f (start,_dest) = isColor Black (get start board)

-- Black and white give the same moves, but pawns inverted


--------------------------------------------------------------------------------
-- King, Queen, Rook and Bishop (-) - move direction is valid
--------------------------------------------------------------------------------

prop_kingOnlyMovesStraight :: Board -> (Int,Int) -> Bool
prop_kingOnlyMovesStraight = pieceOnlyMoves King isMoveStraight

prop_kingOnlyMovesOneStep :: Board -> (Int,Int) -> Bool
prop_kingOnlyMovesOneStep = pieceOnlyMoves King isMoveOneStep

prop_queenOnlyMovesStraight :: Board -> (Int,Int) -> Bool
prop_queenOnlyMovesStraight = pieceOnlyMoves Bishop isMoveStraight

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
pieceOnlyMoves kind isOkay board pos = all isOkay moves
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
-- Queen, Rook and Bishop (-) - empty between start and dest
--------------------------------------------------------------------------------

prop_queenEmptyBetweenStartAndDest :: Board -> (Int,Int) -> Bool
prop_queenEmptyBetweenStartAndDest = emptyBetweenStartAndDest Queen

prop_rookEmptyBetweenStartAndDest :: Board -> (Int,Int) -> Bool
prop_rookEmptyBetweenStartAndDest = emptyBetweenStartAndDest Rook

prop_bishopEmptyBetweenStartAndDest :: Board -> (Int,Int) -> Bool
prop_bishopEmptyBetweenStartAndDest = emptyBetweenStartAndDest Bishop

emptyBetweenStartAndDest :: Kind -> Board -> (Int,Int) -> Bool
emptyBetweenStartAndDest kind board pos = all (allEmpty board') moves
  where
    (board', _pos', moves) = placePieceAndGetMoves board pos kind


--------------------------------------------------------------------------------
-- King, Queen, Rook and Bishop (+) moves if can
--------------------------------------------------------------------------------

prop_kingMovesIfCan :: Board -> (Int,Int) -> Int -> Int -> Property
prop_kingMovesIfCan = movesIfCan getDirKingQueen King 1

prop_queenMovesIfCan :: Board -> (Int,Int) -> Int -> Int -> Property
prop_queenMovesIfCan = movesIfCan getDirKingQueen Queen 7

prop_rookMovesIfCan :: Board -> (Int,Int) -> Int -> Int -> Property
prop_rookMovesIfCan = movesIfCan getDirRook Rook 7

prop_bishopMovesIfCan :: Board -> (Int,Int) -> Int -> Int -> Property
prop_bishopMovesIfCan = movesIfCan getDirBishop Bishop 7

getDirKingQueen :: Int -> (Int,Int)
getDirKingQueen dir = case dir `mod` 8 of
                        0 -> (0,1)  -- Right
                        1 -> (1,0)  -- Down
                        2 -> (0,-1) -- Left
                        3 -> (-1,0) -- Up
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
  allEmpty board' move ==>
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
-- Pawn (-)
--------------------------------------------------------------------------------

-- prop_pawnMovesOne :: Board -> (Int,Int) -> Bool
-- prop_pawnMovesOne board pos = all isMoveOneStep movesPawn
--   where
--     pos'           = withinizePos pos
--     boardWithPawn  = set pos' (Piece Black Pawn) board
--     moves          = movesForColor Black boardWithPawn
--     movesPawn      = filter (isMoveFrom pos') moves

prop_pawnNotMovesCenterIfOther :: Board -> (Int,Int) -> Property
prop_pawnNotMovesCenterIfOther board pos = isWithinBoard pos'' &&
                                           isOtherColor Black atPos'' ==>
                                           not ((pos',pos'') `elem` movesPawn)
  where
    (boardWithPawn, pos', movesPawn) = pawnMoves board pos
    pos''   = pos' `tupleAdd` (1,0)
    atPos'' = get pos'' boardWithPawn


--------------------------------------------------------------------------------
-- Pawn (+)
--------------------------------------------------------------------------------

prop_pawnMovesForwardIfEmpty :: Board -> (Int,Int) -> Property
prop_pawnMovesForwardIfEmpty board pos = isWithinBoard pos'' &&
                                        isEmpty atPos'' ==>
                                        (pos',pos'') `elem` movesPawn
  where
    (boardWithPawn, pos', movesPawn) = pawnMoves board pos
    pos''   = pos' `tupleAdd` (1,0)
    atPos'' = get pos'' boardWithPawn

prop_pawnMovesDobuleIfEmptyAndStart :: Board -> Int -> Property
prop_pawnMovesDobuleIfEmptyAndStart board col = isWithinBoard pos1 &&
                                                isWithinBoard pos2 &&
                                                isEmpty atPos1 &&
                                                isEmpty atPos2 ==>
                                                (pos',pos2) `elem` movesPawn

  where
    row = 1
    (boardWithPawn, pos', movesPawn) = pawnMoves board (row,col)
    pos1   = pos' `tupleAdd` (1,0)
    pos2   = pos' `tupleAdd` (2,0)
    atPos1 = get pos1 boardWithPawn
    atPos2 = get pos2 boardWithPawn

prop_pawnMovesRightIfOtherColor :: Board -> (Int,Int) -> Property
prop_pawnMovesRightIfOtherColor board pos = isWithinBoard pos'' &&
                                            isOtherColor Black atPos'' ==>
                                            (pos',pos'') `elem` movesPawn
  where
    (boardWithPawn, pos', movesPawn) = pawnMoves board pos
    pos''   = pos' `tupleAdd` (1,1)
    atPos'' = get pos'' boardWithPawn

prop_pawnMovesLeftIfOtherColor :: Board -> (Int,Int) -> Property
prop_pawnMovesLeftIfOtherColor board pos = isWithinBoard pos'' &&
                                           isOtherColor Black atPos'' ==>
                                           (pos',pos'') `elem` movesPawn
  where
    (boardWithPawn, pos', movesPawn) = pawnMoves board pos
    pos''   = pos' `tupleAdd` (1,-1)
    atPos'' = get pos'' boardWithPawn

--------------------------------------------------------------------------------
-- Pawn helper functions
--------------------------------------------------------------------------------

pawnMoves :: Board -> (Int,Int) -> (Board, (Int,Int), [((Int,Int),(Int,Int))])
pawnMoves board pos = (boardWithPawn, pos', movesPawn)
  where
    pos'           = withinizePos pos
    boardWithPawn  = set pos' (Piece Black Pawn) board
    moves          = movesForColor Black boardWithPawn
    movesPawn      = filter (isMoveFrom pos') moves

--------------------------------------------------------------------------------
-- General helper functions
--------------------------------------------------------------------------------

verifyMoves :: Board -> [((Int, Int), (Int, Int))] -> Property
verifyMoves board expectedMoves = counterexample errorString verificationResult
  where
    expectedMoves'     = sort expectedMoves
    actualMoves        = sort $ movesForColor Black board
    verificationResult = expectedMoves' == actualMoves
    actualMissing      = expectedMoves' \\ actualMoves
    actualExtra        = actualMoves    \\ expectedMoves'
    errorString        = show board ++ "\n" ++
                         "Expected moves: " ++ show expectedMoves' ++ "\n" ++
                         "Actual moves:   " ++ show actualMoves ++ "\n" ++
                         "Actual is missing: " ++ show actualMissing ++ "\n" ++
                         "Actual has these extra: " ++ show actualExtra

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

allEmpty :: Board -> ((Int,Int),(Int,Int)) -> Bool
allEmpty board (start,dest) = and [isEmpty (get p board) | p <- between]
  where
    diff    = dest `tupleSub` start
    dir     = tupleSignum diff
    length  = tupleMaxAbs diff
    between = map ((start `tupleAdd`) . (dir `tupleMul`)) [1..length-1]

--------------------------------------------------------------------------------
-- Small/misc helper functions
--------------------------------------------------------------------------------

movesFrom :: (Int,Int) -> [(Int,Int)] -> [((Int,Int),(Int,Int))]
movesFrom start ends = map (\end -> (start, end)) ends

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
