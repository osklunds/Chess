
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
import Types as T
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
    legalMoves       = movesFun turn board
    newBoard         = T.applyMove move board
    threatensCurKing = GR.isKingThreatened turn newBoard

applyMove :: Move -> GameState -> (GameState, Result)
applyMove move (GameState {board, turn, captured}) = (nextGameState, result)
  where
    -- Next game state
    capturedThisTurn = moveToCaptured move board
    captured' = case capturedThisTurn of
                  Empty -> captured
                  _else -> capturedThisTurn:captured
    board'    = T.applyMove move $ board
    opponent  = invert turn

    nextGameState = GameState { board    = board'
                              , turn     = opponent
                              , captured = captured'}

    -- Determining result
    result = GR.gameResult opponent board'

moveToCaptured :: Move -> Board -> Square
moveToCaptured (NormalMove _src dst) board = getB dst board
-- TODO: It's hacky to use Empty for this
moveToCaptured _move _board = Empty
    
