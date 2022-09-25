
module Moves.Naive.TestLib
( verifyMoves
, prop_blackAndWhiteGiveSameMoves
)
where

import Test.QuickCheck
import Data.List

import Board
import Moves.Common
import Lib

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

verifyMoves :: (Ord move, Show move, Show board) =>
              (color -> board -> [move]) -> [move] -> color -> board -> Property
verifyMoves movesF expMoves' color board =
    counterexample errorString verificationResult
    where
        expMoves           = sort expMoves'
        actMoves           = sort $ movesF color board
        verificationResult = expMoves == actMoves
        actualMissing      = expMoves \\ actMoves
        actualExtra        = actMoves \\ expMoves
        errorString        = show board ++ "\n" ++
                             "Expected moves: " ++ show expMoves ++ "\n\n" ++
                             "Actual moves: " ++ show actMoves ++ "\n\n" ++
                             "Actual is missing: " ++ show actualMissing ++ "\n\n" ++
                             "Actual has these extra: " ++ show actualExtra

--------------------------------------------------------------------------------
-- Black and White give the same moves
--------------------------------------------------------------------------------

prop_blackAndWhiteGiveSameMoves :: MovesFun -> Board -> Bool
prop_blackAndWhiteGiveSameMoves movesF board =
        blackMoves `listEq` mirroredWhiteMoves
    where
        blackMoves         = movesF Black board
        swappedColors      = swapColors board
        mirroredBoard      = mirrorBoard swappedColors
        whiteMoves         = movesF White mirroredBoard
        mirroredWhiteMoves = map mirrorMove whiteMoves

swapColors :: Board -> Board
swapColors = mapB swapColor

swapColor :: Square -> Square
swapColor Empty = Empty
swapColor (Piece White kind) = Piece Black kind
swapColor (Piece Black kind) = Piece White kind

mirrorBoard :: Board -> Board
mirrorBoard board = foldl mirrorBoardAtPos
                          board
                          [Pos row col | row <- [0..3], col <- [0..7]]

mirrorBoardAtPos :: Board -> Pos -> Board
mirrorBoardAtPos board pos = setB pos atMirroredPos $
                             setB mirroredPos atPos $ board
    where
        mirroredPos   = mirrorPos pos
        atPos         = getB pos board
        atMirroredPos = getB mirroredPos board

mirrorPos :: Pos -> Pos
mirrorPos (Pos row col) = Pos (7 - row) col

mirrorMove :: Move -> Move
mirrorMove (NormalMove src dst) = NormalMove (mirrorPos src) (mirrorPos dst)
mirrorMove (Promote pos kind) = Promote (mirrorPos pos) kind
