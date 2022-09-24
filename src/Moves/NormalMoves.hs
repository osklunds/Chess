
-- Generation of all syntactially possible normal moves.

module Moves.NormalMoves
( movesForColor
)
where

import Board as B hiding (getB)
import qualified Board as B
import Moves.NormalMoves.Lib


movesForColor :: Color -> Board -> [((Int,Int),(Int,Int))]
movesForColor color board = concat [movesFromPos (row,col) color board |
                                    row <- [0..7], col <- [0..7]]

movesFromPos :: (Int,Int) -> Color -> Board -> [((Int,Int),(Int,Int))]
movesFromPos pos color board
  | isColor color atPos = movesFun pos board
  | otherwise           = []
  where
    atPos = getB pos board
    movesFun = case atPos of
                 (Piece _ King )  -> kingMoves
                 (Piece _ Queen)  -> queenMoves
                 (Piece _ Rook)   -> rookMoves
                 (Piece _ Bishop) -> bishopMoves
                 (Piece _ Knight) -> knightMoves
                 (Piece _ Pawn)   -> pawnMoves

kingMoves :: (Int,Int) -> Board -> [((Int,Int),(Int,Int))]
kingMoves pos board = filter isOneStep $ queenMoves pos board
  where
    isOneStep (start,dest) = tupleMaxAbs (dest `tupleSub` start) <= 1

queenMoves :: (Int,Int) -> Board -> [((Int,Int),(Int,Int))]
queenMoves = movesFromDirs queenDirs

rookMoves :: (Int,Int) -> Board -> [((Int,Int),(Int,Int))]
rookMoves = movesFromDirs rookDirs

bishopMoves :: (Int,Int) -> Board -> [((Int,Int),(Int,Int))]
bishopMoves = movesFromDirs bishopDirs

movesFromDirs :: [(Int,Int)] -> (Int,Int) -> Board -> [((Int,Int),(Int,Int))]
movesFromDirs dirs start board = concatMap movesFun dirs
  where
    atPos         = getB start board
    color         = B.color atPos
    movesFun diff = movesFromColorAndDiff color diff start board

movesFromColorAndDiff :: Color ->
                         (Int,Int) ->
                         (Int,Int) ->
                         Board ->
                         [((Int,Int),(Int,Int))]
movesFromColorAndDiff color diff start board
  | not (isWithinBoard dest)  = []
  | isEmpty atDest            = (thisMove:remainingMoves)
  | isOtherColor color atDest = [thisMove]
  | otherwise                 = []
  where
    dest           = start `tupleAdd` diff
    atDest         = getB dest board
    thisMove       = (start,dest)
    remainingMoves = movesFromColorAndDiff color nextDiff start board
    nextDiff       = diff `tupleAdd` tupleSignum diff

queenDirs :: [(Int,Int)]
queenDirs = rookDirs ++ bishopDirs

rookDirs :: [(Int,Int)]
rookDirs = [(1,0),  -- Down
            (0,-1), -- Left
            (-1,0), -- Up
            (0,1)]  -- Right

bishopDirs :: [(Int,Int)]
bishopDirs = [(1,1),   -- Down-right
              (1,-1),  -- Down-left
              (-1,-1), -- Up-left
              (-1,1)]  -- Up-right

knightMoves :: (Int,Int) -> Board -> [((Int,Int),(Int,Int))]
knightMoves start board = filter hasValidDest moves
  where
    (Piece color Knight) = getB start board
    moves                = map (\diff -> (start, start `tupleAdd` diff))
                               knightDiffs
    hasValidDest (_start,dest) = let atDest = getB dest board
                                 in  isWithinBoard dest &&
                                     not (isColor color atDest)

knightDiffs = [(2,1),  -- L
               (2,-1),
               (1,-2),
               (-1,-2),
               (-2,-1),
               (-2,1),
               (1,2),
               (-1,2)]

pawnMoves :: (Int,Int) -> Board -> [((Int,Int),(Int,Int))]
pawnMoves start board = [(start,dest) | dest <- dests]
  where
    color       = B.color $ getB start board
    forwardDir  = case color of
                    Black -> 1
                    White -> (-1)
    (row,_col)  = start
    isAtInitial = case color of
                    Black -> row == 1
                    White -> row == 6

    left          = start `tupleAdd` (forwardDir, -1)
    right         = start `tupleAdd` (forwardDir,  1)
    forward       = start `tupleAdd` (forwardDir,  0)
    doubleForward = start `tupleAdd` (forwardDir*2,0)

    atLeft          = getB left  board
    atRight         = getB right board
    atForward       = getB forward board
    atDoubleForward = getB doubleForward board

    dests = [left    | isWithinBoard left    && isOtherColor color atLeft] ++
            [right   | isWithinBoard right   && isOtherColor color atRight] ++
            [forward | isWithinBoard forward && isEmpty atForward] ++
            [doubleForward | isWithinBoard doubleForward &&
                             isEmpty atDoubleForward &&
                             isEmpty atForward &&
                             isAtInitial]

