
-- Generation of all syntactially possible moves.

module Moves.Naive.CheckUnaware
( movesF
)
where

import Types as T
import Moves.Common
import Moves.Naive.NormalMoves.Lib hiding (getB)
import Debug.Trace

movesF :: MovesFun
movesF = concatApply [normalMoves, promotes, castlings]

concatApply :: [MovesFun] -> MovesFun
concatApply fs c b = concat [f c b | f <- fs]

normalMoves :: MovesFun
normalMoves c b = map f $  movesF' c b
    where
        f ((rowS,colS),(rowD,colD)) = NormalMove (Pos rowS colS) (Pos rowD colD)

movesF' :: Color -> Board -> [((Int,Int),(Int,Int))]
movesF' color board = concat [movesFromPos (row,col) color board |
                              row <- [0..7], col <- [0..7]]

movesFromPos :: (Int,Int) -> Color -> Board -> [((Int,Int),(Int,Int))]
movesFromPos pos color board
  | isColor color atPos = movesFun pos board
  | otherwise           = []
  where
    atPos = getBTemp pos board
    movesFun = case atPos of
                 (Piece _ King )  -> kingMoves
                 (Piece _ Queen)  -> queenMoves
                 (Piece _ Rook)   -> rookMoves
                 (Piece _ Bishop) -> bishopMoves
                 (Piece _ Knight) -> knightMoves
                 (Piece _ Pawn)   -> pawnMoves isWithinPawnArea

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
    atPos         = getBTemp start board
    color         = T.color atPos
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
    atDest         = getBTemp dest board
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
    (Piece color Knight) = getBTemp start board
    moves                = map (\diff -> (start, start `tupleAdd` diff))
                               knightDiffs
    hasValidDest (_start,dest) = let atDest = getBTemp dest board
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

pawnMoves :: ((Int,Int) -> Bool) -> (Int,Int) -> Board -> [((Int,Int),(Int,Int))]
pawnMoves isValidDst start board = [(start,dest) | dest <- dests]
  where
    color       = T.color $ getBTemp start board
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

    atLeft          = getBTemp left  board
    atRight         = getBTemp right board
    atForward       = getBTemp forward board
    atDoubleForward = getBTemp doubleForward board

    dests = [left    | isValidDst left    && isOtherColor color atLeft] ++
            [right   | isValidDst right   && isOtherColor color atRight] ++
            [forward | isValidDst forward && isEmpty atForward] ++
            [doubleForward | isValidDst doubleForward &&
                             isEmpty atDoubleForward &&
                             isEmpty atForward &&
                             isAtInitial]

promotes :: MovesFun
promotes c b = [Promote (toPos src) (toPos dst) kind | (src,dst) <- normals, kind <- [Rook, Bishop, Knight, Queen]]
    where
    -- TODO: Re-use from normal moves
        row = if c == White then 1 else 6
        positions = [Pos row col | col <- [0..7]]
        positionsWithPawn = [p | p <- positions, let atP = getB p b, isPawn atP, isColor c atP]
        normals = concat [pawnMoves isWithinBoard (posToTuple src) b | src <- positionsWithPawn]

-- TODO: Remove
posToTuple :: Pos -> (Int,Int)
posToTuple (Pos row col) = (row, col)


castlings :: MovesFun
castlings = concatApply [kingSideCastle, queenSideCastle]

kingSideCastle :: MovesFun
kingSideCastle c b
    | getBL [Pos row col | col <- [4..7]] b == lane = [Castle c KingSide]
    | otherwise                                     = []
    where
        row = homeRow c
        lane = [Piece c King, Empty, Empty, Piece c Rook]

queenSideCastle :: MovesFun
queenSideCastle c b
    | getBL [Pos row col | col <- [0..4]] b == lane = [Castle c QueenSide]
    | otherwise                                     = []
    where
        row = homeRow c
        lane = [Piece c Rook, Empty, Empty, Empty, Piece c King]

getBL :: [Pos] -> Board -> [Square]
getBL ps b = [getB p b | p <- ps]

-- TODO: Remove
getBTemp :: (Int,Int) -> Board -> Square
getBTemp (row,col) = getB (Pos row col)

