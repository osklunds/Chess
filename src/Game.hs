
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


newGameState :: Board -> GameState
newGameState board = GameState { board, turn = White, captured = []}

validateMove :: Move -> GameState -> ValidationResult
validateMove move (GameState {board, turn})
  | not $ move `elem` legalMoves = IllegalMove
  | threatensCurKing             = WouldBeInCheck
  | otherwise                    = Ok
  where
    legalMoves       = movesF turn board
    newBoard         = B.applyMove move board
    threatensCurKing = GR.isKingThreatened turn newBoard

applyMove :: Move -> GameState -> (GameState, Result)
applyMove move (GameState {board, turn, captured}) = (nextGameState, result)
  where
    -- Next game state
    atDest    = getB (moveToDest move) board -- TODO: Fix when not normal
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
