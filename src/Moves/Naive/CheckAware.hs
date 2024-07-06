
-- Generation of all possible moves, when taking into account that
-- you may not be in check afterwards.

module Moves.Naive.CheckAware
( movesFun
, threatensKing
)
where

import Types
import qualified Moves.Naive.CheckUnaware as CU
import Moves.Common
import Control.Exception
import Debug.Trace


movesFun :: MovesFun
movesFun board = filter isAllowed $ CU.movesFun board
    where
        isAllowed move = isMoveAllowed move board

isMoveAllowed :: Move -> Board -> Bool
isMoveAllowed (Castle color side) = isCastleAllowed color side
isMoveAllowed move                = isOtherAllowed move

isCastleAllowed :: Color -> Side -> Board -> Bool
isCastleAllowed castleColor side board = assert (castleColor == turn) allowed
    where
        turn = getTurn board
        attPoss = attackedPositions $ setTurn (invert turn) board 
        kingPoss = castleToKingPoss castleColor side
        allowed = not $ any (`elem` kingPoss) attPoss

-- Read as "Positions the player who's turn it is can attack"
attackedPositions :: Board -> [Pos]
attackedPositions board = dests
    where
        moves = CU.movesFun board
        dests = concatMap moveToCapturedPoss moves

moveToCapturedPoss :: Move -> [Pos]
moveToCapturedPoss (NormalMove _src dst) = [dst]
moveToCapturedPoss (Promote _src dst _kind) = [dst]
moveToCapturedPoss (Castle _color _side) = []

castleToKingPoss :: Color -> Side -> [Pos]
castleToKingPoss color side = [Pos row (kingCol `op` delta) | delta <- [0..2]]
    where
        row = homeRow color
        kingCol = 4
        op = case side of
                    KingSide -> (+)
                    QueenSide -> (-)

isOtherAllowed :: Move -> Board -> Bool
isOtherAllowed move board = not $ threatensKing newBoard
    where
        newBoard = applyMove move board

-- Read as "the player who's turn it is now threatens its opponents's king"
threatensKing :: Board -> Bool
threatensKing board = any (== Piece (invert turn) King) destSquares
    where
        turn = getTurn board
        attPoss = attackedPositions board
        destSquares = map (\dest -> getB dest board) attPoss
