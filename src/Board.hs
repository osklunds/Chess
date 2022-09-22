
-- Types and operations for boards and pieces.

{-# LANGUAGE LambdaCase #-}

module Board
( 
-- Types
  Board
, defaultBoard
, Square(..)
, Color(..)
, Kind(..)
, Pos(..)
, Move(..)

-- Board operations
, getB
, setB
, foldB
, concatB
, mapB
, anyB
, applyMove

-- Squares
, color
, invert
, isEmpty
, isOccupied
, isColor
, isOtherColor
, isPawn
, isBishop
, isKnight
, isRook
, isQueen
, isKing
)
where

import Control.Monad
import Control.Exception
import Data.Char
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

newtype Board = Board [[Square]]
              deriving (Eq, Ord)

data Square = Empty
            | Piece Color Kind
            deriving (Eq, Ord)

data Color = Black
           | White
           deriving (Eq, Show, Ord)

data Kind = Pawn
          | Bishop
          | Knight
          | Rook
          | Queen
          | King
          deriving (Eq, Show, Ord)

data Pos = Pos Int Int
         deriving (Eq, Show, Ord)

data Move = NormalMove Pos Pos
          | Promote Pos Kind   -- TODO
          | Castle Pos         -- TODO
          | EnPassant Pos Pos  -- TODO

--------------------------------------------------------------------------------
-- Default, Show and Read
--------------------------------------------------------------------------------

defaultBoard :: Board
defaultBoard = Board $ [map (\kind -> Piece Black kind) defaultRow] ++
                       [replicate 8 $ Piece Black Pawn] ++
                       (replicate 4 $ replicate 8 $ Empty) ++
                       [replicate 8 $ Piece White Pawn] ++
                       [map (\kind -> Piece White kind) defaultRow]

defaultRow = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]


instance Show Board where
  show = showBoard

showBoard :: Board -> String
showBoard (Board rows) = "  a b c d e f g h\n" ++
                         (concatMap showRowAndIndex rowsAndIndexes) ++
                         "  a b c d e f g h"
  where
    rowsAndIndexes = zip rows [8,7..1]

showRowAndIndex :: ([Square], Int) -> String
showRowAndIndex (row, i) = show i ++
                           " " ++
                           concat [show sq ++ " " | sq <- row] ++
                           show i ++
                           "\n"

instance Show Square where
  show = showSquare

showSquare :: Square -> String
showSquare Empty                = " "
showSquare (Piece Black Pawn)   = "♟"
showSquare (Piece Black Bishop) = "♝"
showSquare (Piece Black Knight) = "♞"
showSquare (Piece Black Rook)   = "♜"
showSquare (Piece Black Queen)  = "♛"
showSquare (Piece Black King)   = "♚"
showSquare (Piece White Pawn)   = "♙"
showSquare (Piece White Bishop) = "♗"
showSquare (Piece White Knight) = "♘"
showSquare (Piece White Rook)   = "♖"
showSquare (Piece White Queen)  = "♕"
showSquare (Piece White King)   = "♔"


instance Read Board where
  readsPrec _ str = [(fromString str, "")]

fromString :: String -> Board
fromString board = Board $ map f rows''
  where
    rows   = lines board
    rows'  = tail rows
    rows'' = init rows'

    f row  = [fromChar c | (c,i) <- rowWithIndexes,
                           not (isDigit c),
                           even i]
      where
        rowWithIndexes = zip row [0..]

fromChar :: Char -> Square
fromChar ' ' = Empty
fromChar '♟' = Piece Black Pawn
fromChar '♝' = Piece Black Bishop
fromChar '♞' = Piece Black Knight
fromChar '♜' = Piece Black Rook
fromChar '♛' = Piece Black Queen
fromChar '♚' = Piece Black King
fromChar '♙' = Piece White Pawn
fromChar '♗' = Piece White Bishop
fromChar '♘' = Piece White Knight
fromChar '♖' = Piece White Rook
fromChar '♕' = Piece White Queen
fromChar '♔' = Piece White King


--------------------------------------------------------------------------------
-- Arbitrary
--------------------------------------------------------------------------------

instance Arbitrary Kind where
  arbitrary = elements [Pawn, Bishop, Knight, Rook, Queen, King]

instance Arbitrary Color where
  arbitrary = elements [Black, White]

instance Arbitrary Square where
  arbitrary = oneof [return Empty,
                     do
                      color <- arbitrary
                      kind  <- arbitrary
                      return $ Piece color kind
                    ]

instance Arbitrary Pos where
    arbitrary = do
        row <- oneof $ map return [0..7]
        col <- oneof $ map return [0..7]
        return $ Pos row col

instance Arbitrary Board where
  arbitrary = do
    rows <- replicateM 8 $ replicateM 8 arbitrary

    let board   = Board rows
    let noKings = mapB (\case
                           (Piece _color King) -> Empty
                           square              -> square
                       ) board
    positions <- replicateM 4 $ arbitrary
    let [row1,col1,row2,col2] = map (`mod` 8) positions
    let col2' = case (row1,col1) == (row2,col2) of
                    True  -> (col2 + 1) `mod` 8
                    False -> col2

    let blackKing = setB (Pos row1 col1)  (Piece Black King) noKings
    let whiteKing = setB (Pos row2 col2') (Piece White King) blackKing

    return whiteKing


--------------------------------------------------------------------------------
-- Board operations
--------------------------------------------------------------------------------

getB :: Pos -> Board -> Square
getB (Pos row col) (Board board) = (board !! row) !! col

setB :: Pos -> Square -> Board -> Board
setB (Pos rowIdx colIdx) sq (Board oldBoard) = Board newBoard
  where
    oldRow = oldBoard !! rowIdx
    newRow = replaceAt colIdx sq oldRow
    newBoard = replaceAt rowIdx newRow oldBoard

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs = prefix ++ [x] ++ suffix
  where
    (prefix,(_:suffix)) = splitAt i xs

foldB :: (a -> Square -> a) -> a -> Board -> a
foldB f v board = foldl f v $ concatB board

concatB :: Board -> [Square]
concatB (Board rows) = concat rows

mapB :: (Square -> Square) -> Board -> Board
mapB f (Board rows) = Board $ map (\row -> map f row) rows

anyB :: (Square -> Bool) -> Board -> Bool
anyB f = foldB (\acc square -> acc || f square) False

applyMove :: Move -> Board -> Board
applyMove (NormalMove src dst) b = applyNormalMove src dst b
applyMove (Promote p kind) b = applyPromote p kind b
applyMove _otherMove _b = undefined

applyNormalMove src dst b = assert (not $ isEmpty atSrc)
                                   (setB dst atSrc $ setB src Empty b)
    where
        atSrc = getB src b

applyPromote :: Pos -> Kind -> Board -> Board
applyPromote p kind b = assert condition setB p newAtP b
    where
        -- TODO: assert row+color, kind
        atP    = getB p b
        newAtP = Piece (color atP) kind

        condition = pawnAtP && notToPawnOrKing && pAtTopOrBottom
        pawnAtP = isPawn atP
        notToPawnOrKing = kind /= Pawn && kind /= King
        pAtTopOrBottom = row == 0 && color atP == White ||
                         row == 7 && color atP == Black
        (Pos row _col) = p

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

color :: Square -> Color
color (Piece c _) = c

invert :: Color -> Color
invert White = Black
invert Black = White

isEmpty :: Square -> Bool
isEmpty Empty = True
isEmpty _     = False

isOccupied :: Square -> Bool
isOccupied = not . isEmpty

isColor :: Color -> Square -> Bool
isColor _  Empty        = False
isColor c1 (Piece c2 _) = c1 == c2

isOtherColor :: Color -> Square -> Bool
isOtherColor _  Empty        = False
isOtherColor c1 (Piece c2 _) = c1 /= c2

isPawn :: Square -> Bool
isPawn (Piece _ Pawn) = True
isPawn _              = False

isBishop :: Square -> Bool
isBishop (Piece _ Bishop) = True
isBishop _                = False

isKnight :: Square -> Bool
isKnight (Piece _ Knight) = True
isKnight _                = False

isRook :: Square -> Bool
isRook (Piece _ Rook) = True
isRook _              = False

isQueen :: Square -> Bool
isQueen (Piece _ Queen) = True
isQueen _               = False

isKing :: Square -> Bool
isKing (Piece _ King) = True
isKing _              = False
