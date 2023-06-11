
-- Generation of all syntactially possible moves.

module Moves.Naive.CheckUnaware
( movesFun
)
where

import Types
import Moves.Common
import Debug.Trace

movesFun :: MovesFun
movesFun = concatApply [normalAndPromotesMovesFun, castlingsMovesFun]

concatApply :: [MovesFun] -> MovesFun
concatApply movesFuns color board = concat [movesFun color board | movesFun <- movesFuns]

normalAndPromotesMovesFun :: MovesFun
normalAndPromotesMovesFun color board = concat [movesFromPos (Pos row col) color board |
                                                row <- [0..7], col <- [0..7]]

movesFromPos :: Pos -> Color -> Board -> [Move]
movesFromPos pos color board
    | isColor color atPos = movesFun pos board
    | otherwise = []
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
    -- searching for more moves in this direction.
    | not (isWithinBoard dst) = []

    -- The diff points at an empty square. We can move there, and there are
    -- more potential moves, so let's continue searching.
    | isEmpty atDst = (thisMove:remainingMoves)

    -- The diff points at a square with a piece of the other color. We can move
    -- there to capture it. But we can't jump over it, so stop searching for
    -- more moves in this direction.
    | isOtherColor color atDst = [thisMove]

    -- The diff points at a square with a piece of the same color. We can't
    -- move there. And we can't jump over it either, so stop searching for more
    -- moves in this direction.
    | isColor color atDst = []
    where
        dst = src `posPlusDiff` diff
        atDst = getB dst board
        thisMove = (src,dst)
        nextDiff  = expandDiffByOne diff
        remainingMoves = movesFromColorAndDiff color nextDiff src board

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

knightMoves :: Pos -> Board -> [Move]
knightMoves src board = filter hasValidDst moves
    where
        color = colorOf $ getB src board
        moves = map (\diff -> NormalMove src (src `posPlusDiff` diff)) knightDiffs
        hasValidDst (NormalMove _src dst) = isWithinBoard dst &&
                                            not (isColor color (getB dst board))

knightDiffs :: [Diff]
knightDiffs = [downRight, downLeft, leftDown, leftUp, upLeft, upRight, rightDown, rightUp]
    where
        downRight = Diff 2    1
        downLeft  = Diff 2    (-1)
        leftDown  = Diff 1    (-2)
        leftUp    = Diff (-1) (-2)
        upLeft    = Diff (-2) (-1)
        upRight   = Diff (-2) 1
        rightDown = Diff 1    2
        rightUp   = Diff (-1) 2

pawnMoves :: Pos -> Board -> [Move]
pawnMoves src board = moves
    where
        color = colorOf $ getB src board
        forwardDir = pawnForwardDir color

        left          = src `posPlusDiff` Diff forwardDir     (-1)
        right         = src `posPlusDiff` Diff forwardDir     1
        forward       = src `posPlusDiff` Diff forwardDir     0
        doubleForward = src `posPlusDiff` Diff (forwardDir*2) 0

        atLeft          = getB left board
        atRight         = getB right board
        atForward       = getB forward board
        atDoubleForward = getB doubleForward board

        row = rowOf src
        isAtPawnHomeRow = row == pawnHomeRow color

        dsts = [left          | isWithinBoard left          && isOtherColor color atLeft                                      ] ++
               [right         | isWithinBoard right         && isOtherColor color atRight                                     ] ++
               [forward       | isWithinBoard forward       && isEmpty atForward                                              ] ++
               [doubleForward | isWithinBoard doubleForward && isEmpty atDoubleForward && isEmpty atForward && isAtPawnHomeRow]

        goalRow = homeRow $ invert color
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

pawnForwardDir :: Color -> Int
pawnForwardDir Black = 1
pawnForwardDir White = -1

-- TODO: Improve variable names
castlingsMovesFun :: MovesFun
castlingsMovesFun = concatApply [kingSideCastle, queenSideCastle]

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


-------------------------------------------------------------------------------
-- Diff
-------------------------------------------------------------------------------

data Diff = Diff Int Int deriving (Eq, Show, Ord)

posDiff :: Pos -> Pos -> Diff
posDiff (Pos srcRow srcCol) (Pos dstRow dstCol) =
    Diff (srcRow - dstRow) (srcCol - dstCol)

diffMaxAbs :: Diff -> Int
diffMaxAbs (Diff row col) = max (abs row) (abs col)

posPlusDiff :: Pos -> Diff -> Pos
posPlusDiff (Pos row col) (Diff rowDiff colDiff) = Pos (row + rowDiff) (col + colDiff)

expandDiffByOne :: Diff -> Diff
expandDiffByOne (Diff rowDiff colDiff) = Diff (rowDiff + signum rowDiff) (colDiff + signum colDiff)

