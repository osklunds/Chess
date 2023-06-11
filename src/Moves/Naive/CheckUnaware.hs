
-- Generation of all syntactially possible moves.

module Moves.Naive.CheckUnaware
( movesFun
)
where

import Types
import Moves.Common
import Debug.Trace

movesFun :: MovesFun
movesFun = concatApply [normalAndPromotes, castlings]

concatApply :: [MovesFun] -> MovesFun
concatApply movesFuns color board = concat [movesFun color board | movesFun <- movesFuns]

normalAndPromotes :: MovesFun
normalAndPromotes color board = concat [movesFromPos (Pos row col) color board |
                                  row <- [0..7], col <- [0..7]]

movesFromPos :: Pos -> Color -> Board -> [Move]
movesFromPos pos color board
    | isColor color atPos = movesFun pos board
    | otherwise           = []
    where
        atPos = getB pos board
        movesFun = case atPos of
                     (Piece _color King )  -> kingMoves
                     (Piece _color Queen)  -> queenMoves
                     (Piece _color Rook)   -> rookMoves
                     (Piece _color Bishop) -> bishopMoves
                     (Piece _color Knight) -> knightMoves
                     (Piece _color Pawn)   -> pawnMoves

kingMoves :: Pos -> Board -> [Move]
kingMoves pos board = filter isOneStep $ queenMoves pos board
  where
    isOneStep (NormalMove src dst) = diffMaxAbs (dst `posDiff` src) <= 1

data Diff = Diff Int Int
          deriving (Eq, Show, Ord)

posDiff :: Pos -> Pos -> Diff
posDiff (Pos srcRow srcCol) (Pos dstRow dstCol) =
    Diff (srcRow - dstRow) (srcCol - dstCol)

diffMaxAbs :: Diff -> Int
diffMaxAbs (Diff row col) = max (abs row) (abs col)

queenMoves :: Pos -> Board -> [Move]
queenMoves = movesFromDirs queenDirs

rookMoves :: Pos -> Board -> [Move]
rookMoves = movesFromDirs rookDirs

bishopMoves :: Pos -> Board -> [Move]
bishopMoves = movesFromDirs bishopDirs

movesFromDirs :: [Diff] -> Pos -> Board -> [Move]
movesFromDirs dirs src board = concatMap movesFun dirs
    where
        atSrc = getB src board
        color = colorOf atSrc
        movesAsTuplesFun diff = movesFromColorAndDiff color diff src board
        movesFun = map (\(src,dst) -> NormalMove src dst) . movesAsTuplesFun

movesFromColorAndDiff :: Color -> Diff -> Pos -> Board -> [(Pos,Pos)]
movesFromColorAndDiff color diff src board
    -- The diff points outside the board. Not a valid move, and no point in
    -- seraching for more moves in this direction.
    | not (isWithinBoard dst) = []

    -- The diff points at an empty square. We can move there, and there are
    -- more potential moves, so let's continue searching.
    | isEmpty atDst = (thisMove:remainingMoves)

    -- The diff points at a square with a piece of the other color. We can move
    -- there to capture it. But we can't jump over it, so stop seraching for
    -- more moves in this direction.
    | isOtherColor color atDst = [thisMove]

    -- The diff points at a square with a piece of the same color. We can't
    -- move there. And we can't jump over it either, so stop seraching for more
    -- moves in this direction.
    | isColor color atDst = []
    where
        dst = src `posPlusDiff` diff
        atDst = getB dst board
        thisMove = (src,dst)
        nextDiff  = expandDiffByOne diff
        remainingMoves = movesFromColorAndDiff color nextDiff src board

posPlusDiff :: Pos -> Diff -> Pos
posPlusDiff (Pos row col) (Diff rowDiff colDiff) = Pos (row + rowDiff) (col + colDiff)

expandDiffByOne :: Diff -> Diff
expandDiffByOne (Diff rowDiff colDiff) = Diff (rowDiff + signum rowDiff) (colDiff + signum colDiff)

queenDirs :: [Diff]
queenDirs = rookDirs ++ bishopDirs

rookDirs :: [Diff]
rookDirs = [down, left, up, right]
    where
        down  = Diff 1    0
        left  = Diff 0    (-1)
        up    = Diff (-1) 0
        right = Diff 0    1

bishopDirs :: [Diff]
bishopDirs = [downRight, downLeft, upLeft, upRight]
    where
        downRight = Diff 1    1
        downLeft  = Diff 1    (-1)
        upLeft    = Diff (-1) (-1)
        upRight   = Diff (-1) 1

-- TODO: Change the Pos tuple to Move
knightMoves :: Pos -> Board -> [Move]
knightMoves src board = filter hasValidDst moves
    where
        (Piece color Knight) = getB src board
        moves = map (\diff -> NormalMove src (src `posPlusDiff` diff)) knightDiffs
        hasValidDst (NormalMove _src dst) = isWithinBoard dst &&
                                            not (isColor color (getB dst board))

knightDiffs :: [Diff]
knightDiffs = [Diff 2    1,
               Diff 2    (-1),
               Diff 1    (-2),
               Diff (-1) (-2),
               Diff (-2) (-1),
               Diff (-2) 1,
               Diff 1    2,
               Diff (-1) 2]

pawnMoves :: Pos -> Board -> [Move]
pawnMoves src board = moves
    where
        color = colorOf $ getB src board
        forwardDir = case color of
                       Black -> 1
                       White -> (-1)
        row = rowOf src
        -- TODO: Use homeRow fun
        isAtHomeRow = case color of
                        Black -> row == 1
                        White -> row == 6

        left          = src `posPlusDiff` Diff forwardDir     (-1)
        right         = src `posPlusDiff` Diff forwardDir     1
        forward       = src `posPlusDiff` Diff forwardDir     0
        doubleForward = src `posPlusDiff` Diff (forwardDir*2) 0

        atLeft          = getB left board
        atRight         = getB right board
        atForward       = getB forward board
        atDoubleForward = getB doubleForward board

        -- TODO: Make this prettier
        dsts = [left | isWithinBoard left && isOtherColor color atLeft] ++
               [right | isWithinBoard right && isOtherColor color atRight] ++
               [forward | isWithinBoard forward && isEmpty atForward] ++
               [doubleForward | isWithinBoard doubleForward && isEmpty atDoubleForward && isEmpty atForward && isAtHomeRow]

        goalRow = case color of
                    Black -> 7
                    White -> 0
        dstAtGoalRow = case dsts of
                        (dst:_rest) -> rowOf dst == goalRow
                        [] -> False
        createMove = case dstAtGoalRow of
                        True ->
                        -- TODO: Assert that all dsts are at goal row
                            \dst -> [Promote src dst kind | kind <- [Rook, Bishop, Knight, Queen]]
                        False ->
                            \dst -> [NormalMove src dst]
        moves = concatMap createMove dsts

-- TODO: Improve variable names
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

