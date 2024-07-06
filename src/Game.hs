
-- Game state and actions on the game.

{-# LANGUAGE NamedFieldPuns #-}

module Game
( GameState
, board
, captured
, newGameState
, ValidationResult(..)
, validateMove
, Game.applyMove
)
where

import Types hiding (applyMove)
import qualified Types
import Moves
import Score
import Moves.Naive.CheckAware (threatensKing)


data GameState = GameState { board    :: Board
                           , captured :: [Square]
                           }

data ValidationResult = Ok | IllegalMove | WouldBeInCheck


newGameState :: Board -> GameState
newGameState board = GameState { board, captured = []}

validateMove :: Move -> GameState -> ValidationResult
validateMove move (GameState { board })
    | not $ move `elem` legalMoves = IllegalMove
    | threatensCurKing             = WouldBeInCheck
    | otherwise                    = Ok
    where
        legalMoves       = movesFun board
        newBoard         = Types.applyMove move board
        threatensCurKing = threatensKing newBoard

applyMove :: Move -> GameState -> (GameState, Result)
applyMove move (GameState {board, captured}) = (nextGameState, result)
    where
        -- Next game state
        capturedThisTurn = moveToCaptured move board
        captured' = case capturedThisTurn of
                      Empty -> captured
                      _else -> capturedThisTurn:captured
        board' = Types.applyMove move $ board

        nextGameState = GameState { board    = board'
                                  , captured = captured'}

        -- Determining result
        (_scoreValue, result, _moves) = score board'

moveToCaptured :: Move -> Board -> Square
moveToCaptured (NormalMove _src dst) board = getB dst board
-- TODO: It's hacky to use Empty for this
moveToCaptured _move _board = Empty
    
