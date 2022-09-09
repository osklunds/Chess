
{-# LANGUAGE NamedFieldPuns #-}

module Game
( GameState
, board
, turn
, captured
, newGameState
, ValidationResult(..)
, validateMove
, Result(..)
, Game.applyMove
)
where

import Board as B
import Moves.CheckAware


data GameState = GameState { board    :: Board
                           , turn     :: Color
                           , captured :: [Square]
                           }

data ValidationResult = Ok | IllegalMove | WouldBeInCheck

data Result = Normal | Check | Checkmate | Draw


newGameState :: GameState
newGameState = GameState { board    = defaultBoard
                         , turn     = White
                         , captured = []}

validateMove :: ((Int,Int),(Int,Int)) -> GameState -> ValidationResult
validateMove move (GameState {board, turn})
  | not $ move `elem` legalMoves = IllegalMove
  | threatensCurKing             = WouldBeInCheck
  | otherwise                    = Ok
  where
    legalMoves       = movesForColor turn board
    newBoard         = B.applyMove move board
    threatensCurKing = isKingThreatened turn newBoard

isKingThreatened :: Color -> Board -> Bool
isKingThreatened color board = any (== Piece color King) destSquares
  where
    movesOther  = movesForColor (invert color) board
    dests       = map snd movesOther
    destSquares = map (\dest -> getB dest board) dests

applyMove :: ((Int,Int),(Int,Int)) -> GameState -> (GameState, Result)
applyMove move (GameState {board, turn, captured}) = (nextGameState, result)
  where
    -- Next game state
    atDest    = getB (snd move) board
    captured' = case atDest of
                  Empty -> captured
                  _else -> atDest:captured
    board'    = B.applyMove move $ board
    opponent  = invert turn

    nextGameState = GameState { board    = board'
                              , turn     = opponent
                              , captured = captured'}

    -- Determining result
    oppMoves             = movesForColor opponent board'
    oppMovesNotCheck     = filter moveNotCheck oppMoves
    moveNotCheck oppMove =
      not $ isKingThreatened opponent $ B.applyMove oppMove board'

    oppCanMoveUnthreatened = not $ null oppMovesNotCheck
    oppIsThreatened        = isKingThreatened opponent board'

    result = case (oppCanMoveUnthreatened, oppIsThreatened) of
                (True , True ) -> Check
                (True , False) -> Normal
                (False, True ) -> Checkmate
                (False, False) -> Draw
