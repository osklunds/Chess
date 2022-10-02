
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
import Control.Exception


movesF :: MovesFun
movesF color board = filter (isMoveAllowed color board attPoss) $
                            CU.movesF color board
    where
        attPoss = attackedPositions (invert color) board

isMoveAllowed :: Color -> Board -> [Pos] -> Move -> Bool
isMoveAllowed c _b aps m@(Castle _color _side) = isCastleAllowed c m aps
isMoveAllowed color board _aps move = not $ isKingThreatened color newBoard
    where
        newBoard = applyMove move board

isCastleAllowed :: Color -> Move -> [Pos] -> Bool
isCastleAllowed color (Castle color' side) attPoss =
        assert (color == color') allowed
    where
        kingPoss = castleToKingPoss color side
        allowed = not $ any (`elem` kingPoss) attPoss

attackedPositions :: Color -> Board -> [Pos]
attackedPositions color board = dests
    where
        moves = CU.movesF color board
        dests = concatMap moveToDests moves

castleToKingPoss :: Color -> Side -> [Pos]
castleToKingPoss color side = [Pos row (kingCol `op` delta) | delta <- [0..2]]
    where
        row = homeRow color
        kingCol = 4
        op = case side of
                    KingSide -> (+)
                    QueenSide -> (-)

isKingThreatened :: Color -> Board -> Bool
isKingThreatened color board = any (== Piece color King) destSquares
    where
        attPoss = attackedPositions (invert color) board
        destSquares = map (\dest -> getB dest board) attPoss

moveToDests :: Move -> [Pos]
moveToDests (NormalMove _src dst) = [dst]
moveToDests (Promote pos _kind) = [pos]
moveToDests (Castle _color _side) = [] -- TODO
