
module Moves
( movesForColor 
)
where

import Board as B

movesForColor :: Color -> Board -> [((Int,Int),(Int,Int))]
movesForColor color board = concat [movesAtPos (row,col) color board |
                                    row <- [0..7], col <- [0..7]]

movesAtPos :: (Int,Int) -> Color -> Board -> [((Int,Int),(Int,Int))]
movesAtPos pos c board
  | B.isColor c atPos = movesFun pos board
  | otherwise = []
  where
    atPos = B.get pos board
    movesFun = case atPos of
                 (Piece _ Queen)  -> queenMoves
                 (Piece _ King )  -> kingMoves
                 (Piece _ Bishop) -> bishopMoves
                 (Piece _ Rook)   -> rookMoves
                 (Piece _ Pawn)   -> pawnMoves
                 _               -> \_ _ -> []
                 --Empty           -> \_ _ -> []

kingMoves :: (Int,Int) -> Board -> [((Int,Int),(Int,Int))]
kingMoves pos board = filter f $ queenMoves pos board
  where
    f (start,dest) = tupleMaxAbs (dest `tupleSub` start) <= 1

queenMoves :: (Int,Int) -> Board -> [((Int,Int),(Int,Int))]
queenMoves pos board = concatMap movesFun (straightDeltas ++ diagonalDeltas)
  where
    atPos    = get pos board
    color    = B.color atPos
    movesFun = (\d -> movesFromDelta board d pos color)

bishopMoves :: (Int,Int) -> Board -> [((Int,Int),(Int,Int))]
bishopMoves pos board = concatMap movesFun diagonalDeltas
  where
    atPos      = get pos board
    color      = B.color atPos
    movesFun d = movesFromDelta board d pos color

rookMoves :: (Int,Int) -> Board -> [((Int,Int),(Int,Int))]
rookMoves pos board = concatMap movesFun straightDeltas
  where
    atPos      = get pos board
    color      = B.color atPos
    movesFun d = movesFromDelta board d pos color

pawnMoves :: (Int,Int) -> Board -> [((Int,Int),(Int,Int))]
pawnMoves pos board = [(pos,dest) | dest <- dests]
  where
    atPos       = get pos board
    color       = B.color atPos
    delta       = case color of
                    Black -> 1
                    White -> (-1)

    posCenter  = pos `tupleAdd` (delta,0)
    posDouble  = pos `tupleAdd` (delta*2,0)
    posLeft    = pos `tupleAdd` (delta,-1)
    posRight   = pos `tupleAdd` (delta,1)

    atCenter    = get posCenter board
    atDouble    = get posDouble board
    atLeft      = get posLeft  board
    atRight     = get posRight board

    (row,_col)  = pos
    isAtInitial = case color of
                    Black -> row == 1
                    White -> row == 6

    dests     = case isWithinBoard posLeft && isOtherColor color atLeft of
                  True  -> [posLeft]
                  False -> []
                ++
                case isWithinBoard posRight && isOtherColor color atRight of
                  True  -> [posRight]
                  False -> []
                ++
                case isWithinBoard posCenter && isEmpty atCenter of
                  True ->  [posCenter]
                  False -> []
                ++
                case isWithinBoard posDouble && isEmpty atCenter &&
                     isEmpty atDouble && isAtInitial of
                  True -> [posDouble]
                  False -> []

straightDeltas :: [(Int,Int)]
straightDeltas = [(1,0),  -- Down
                  (0,-1), -- Left
                  (-1,0), -- Up
                  (0,1)]  -- Right

diagonalDeltas :: [(Int,Int)]
diagonalDeltas = [(1,1),   -- Down-right
                  (1,-1),  -- Down-left
                  (-1,-1), -- Up-left
                  (-1,1)]  -- Up-right

tupleAdd :: (Int,Int) -> (Int,Int) -> (Int,Int)
tupleAdd (a,b) (c,d) = (a+c,b+d)

tupleSub :: (Int,Int) -> (Int,Int) -> (Int,Int)
tupleSub (a,b) (c,d) = (a-c,b-d)

tupleSignum :: (Int,Int) -> (Int,Int)
tupleSignum (a,b) = (signum a,signum b)

tupleMaxAbs :: (Int,Int) -> Int
tupleMaxAbs (a,b) = max (abs a) (abs b)

isWithinBoard :: (Int,Int) -> Bool
isWithinBoard (row,col) = 0 <= row && row < 8 && 0 <= col && col < 8

movesFromDelta :: Board ->
                  (Int,Int) ->
                  (Int,Int) ->
                  Color ->
                  [((Int,Int),(Int,Int))]
movesFromDelta board diff start color
  | not (isWithinBoard pos)    = []
  | B.isEmpty atPos            = (here:rest)
  | B.isOtherColor color atPos = [here]
  | otherwise                  = []
  where
    pos   = start `tupleAdd` diff
    atPos = B.get pos board
    here  = (start,pos)
    rest  = movesFromDelta board diff' start color
    diff' = diff `tupleAdd` tupleSignum diff
