
-- Generation of all possible moves, when taking into account that
-- you may not be in check afterwards.

module Moves.Naive.CheckAware
( movesF
, isKingThreatened
)
where

import Board
import qualified Moves.Naive.CheckUnaware as CU
import Moves.Common


movesF :: MovesFun
movesF color board = filter (isKingSafeAfterMove color board) $
                            CU.movesF color board

isKingSafeAfterMove :: Color -> Board -> Move -> Bool
isKingSafeAfterMove color board move = not $ isKingThreatened color newBoard
    where
        newBoard = applyMove move board

isKingThreatened :: Color -> Board -> Bool
isKingThreatened color board = any (== Piece color King) destSquares
    where
        movesOther  = CU.movesF (invert color) board
        dests       = concatMap moveToDests movesOther
        destSquares = map (\dest -> getB dest board) dests

moveToDests :: Move -> [Pos]
moveToDests (NormalMove _src dst) = [dst]
moveToDests (Promote pos _kind) = [pos]
moveToDests (Castle _color _side) = [] -- TODO
