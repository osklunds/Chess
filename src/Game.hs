
-- Game state and actions on the game.

{-# LANGUAGE NamedFieldPuns #-}

module Game
( GameState
, board
, turn
, captured
, newGameState
, ValidationResult(..)
, validateMove
, Game.applyMove
)
where

import GameResult as GR
import Board as B
import Moves


data GameState = GameState { board    :: Board
                           , turn     :: Color
                           , captured :: [Square]
                           }

data ValidationResult = Ok | IllegalMove | WouldBeInCheck


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
    threatensCurKing = GR.isKingThreatened turn newBoard

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
    result = GR.gameResult opponent board'
