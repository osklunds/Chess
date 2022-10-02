
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
movesF color board = filter isAllowed $ CU.movesF color board
    where
        attPoss = attackedPositions (invert color) board
        isAllowed move = isMoveAllowed move color board attPoss

isMoveAllowed :: Move -> Color -> Board -> [Pos] -> Bool
isMoveAllowed (Castle color side) = isCastleAllowed color side
isMoveAllowed move                = isOtherAllowed move

isCastleAllowed :: Color -> Side -> Color -> Board -> [Pos] -> Bool
isCastleAllowed castleColor side moveColor _board attPoss =
    assert (castleColor == moveColor) allowed
    where
        kingPoss = castleToKingPoss moveColor side
        allowed = not $ any (`elem` kingPoss) attPoss

attackedPositions :: Color -> Board -> [Pos]
attackedPositions color board = dests
    where
        moves = CU.movesF color board
        dests = concatMap moveToCapturedPoss moves

moveToCapturedPoss :: Move -> [Pos]
moveToCapturedPoss (NormalMove _src dst) = [dst]
moveToCapturedPoss (Promote pos _kind) = [pos]
moveToCapturedPoss (Castle _color _side) = []

castleToKingPoss :: Color -> Side -> [Pos]
castleToKingPoss color side = [Pos row (kingCol `op` delta) | delta <- [0..2]]
    where
        row = homeRow color
        kingCol = 4
        op = case side of
                    KingSide -> (+)
                    QueenSide -> (-)

isOtherAllowed :: Move -> Color -> Board -> [Pos] -> Bool
isOtherAllowed move color board _attPoss = not $ isKingThreatened color newBoard
    where
        newBoard = applyMove move board

isKingThreatened :: Color -> Board -> Bool
isKingThreatened color board = any (== Piece color King) destSquares
    where
        attPoss = attackedPositions (invert color) board
        destSquares = map (\dest -> getB dest board) attPoss
