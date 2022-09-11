-- Types and operations for boards and pieces.
{-# LANGUAGE LambdaCase #-}

module Board
    ( Board
    , Square(..)
    , Color(..)
    , Kind(..)
    , defaultBoard
    , getB
    , setB
    , foldB
    , concatB
    , mapB
    , anyB
    , applyMove
    , color
    , invert
    , isEmpty
    , isColor
    , isOtherColor
    , isPawn
    , isBishop
    , isKnight
    , isRook
    , isQueen
    , isKing
    ) where

import Control.Monad
import Data.Char
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
newtype Board =
    Board [[Square]]
    deriving (Eq)

data Square
    = Empty
    | Piece Color Kind
    deriving (Eq)

data Color
    = Black
    | White
    deriving (Eq, Show)

data Kind
    = Pawn
    | Bishop
    | Knight
    | Rook
    | Queen
    | King
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Default, Show and Read
--------------------------------------------------------------------------------
defaultBoard :: Board
defaultBoard =
    Board $
    [map (\kind -> Piece Black kind) defaultRow] ++
    [replicate 8 $ Piece Black Pawn] ++
    (replicate 4 $ replicate 8 $ Empty) ++
    [replicate 8 $ Piece White Pawn] ++
    [map (\kind -> Piece White kind) defaultRow]

defaultRow = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

instance Show Board where
    show = showBoard

showBoard :: Board -> String
showBoard (Board rows) =
    "  a b c d e f g h\n" ++
    (concatMap showRowAndIndex rowsAndIndexes) ++ "  a b c d e f g h"
  where
    rowsAndIndexes = zip rows [8,7 .. 1]

showRowAndIndex :: ([Square], Int) -> String
showRowAndIndex (row, i) =
    show i ++ " " ++ concat [show sq ++ " " | sq <- row] ++ show i ++ "\n"

instance Show Square where
    show = showSquare

showSquare :: Square -> String
showSquare Empty = " "
showSquare (Piece Black Pawn) = "♟"
showSquare (Piece Black Bishop) = "♝"
showSquare (Piece Black Knight) = "♞"
showSquare (Piece Black Rook) = "♜"
showSquare (Piece Black Queen) = "♛"
showSquare (Piece Black King) = "♚"
showSquare (Piece White Pawn) = "♙"
showSquare (Piece White Bishop) = "♗"
showSquare (Piece White Knight) = "♘"
showSquare (Piece White Rook) = "♖"
showSquare (Piece White Queen) = "♕"
showSquare (Piece White King) = "♔"

instance Read Board where
    readsPrec _ str = [(fromString str, "")]

fromString :: String -> Board
fromString board = Board $ map f rows''
  where
    rows = lines board
    rows' = tail rows
    rows'' = init rows'
    f row = [fromChar c | (c, i) <- rowWithIndexes, not (isDigit c), even i]
      where
        rowWithIndexes = zip row [0 ..]

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
    arbitrary =
        oneof
            [ return Empty
            , do color <- arbitrary
                 kind <- arbitrary
                 return $ Piece color kind
            ]

instance Arbitrary Board where
    arbitrary = do
        rows <- replicateM 8 $ replicateM 8 arbitrary
        let board = Board rows
        let noKings =
                mapB
                    (\case
                         (Piece _color King) -> Empty
                         square -> square)
                    board
        positions <- replicateM 4 $ arbitrary
        let [row1, col1, row2, col2] = map (`mod` 8) positions
        let col2' =
                case (row1, col1) == (row2, col2) of
                    True -> (col2 + 1) `mod` 8
                    False -> col2
        let blackKing = setB (row1, col1) (Piece Black King) noKings
        let whiteKing = setB (row2, col2') (Piece White King) blackKing
        return whiteKing

--------------------------------------------------------------------------------
-- Board operations
--------------------------------------------------------------------------------
getB :: (Int, Int) -> Board -> Square
getB (row, col) (Board board) = (board !! row) !! col

setB :: (Int, Int) -> Square -> Board -> Board
setB (rowIndex, colIndex) sq (Board oldBoard) = Board newBoard
  where
    oldRow = oldBoard !! rowIndex
    newRow = replaceAt colIndex sq oldRow
    newBoard = replaceAt rowIndex newRow oldBoard

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs = prefix ++ [x] ++ suffix
  where
    (prefix, (_:suffix)) = splitAt i xs

foldB :: (a -> Square -> a) -> a -> Board -> a
foldB f v board = foldl f v $ concatB board

concatB :: Board -> [Square]
concatB (Board rows) = concat rows

mapB :: (Square -> Square) -> Board -> Board
mapB f (Board rows) = Board $ map (\row -> map f row) rows

anyB :: (Square -> Bool) -> Board -> Bool
anyB f = foldB (\acc square -> acc || f square) False

applyMove :: ((Int, Int), (Int, Int)) -> Board -> Board
applyMove (start, dest) board = setB dest atStart $ setB start Empty board
  where
    atStart = getB start board

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
isEmpty _ = False

isColor :: Color -> Square -> Bool
isColor _ Empty = False
isColor c1 (Piece c2 _) = c1 == c2

isOtherColor :: Color -> Square -> Bool
isOtherColor _ Empty = False
isOtherColor c1 (Piece c2 _) = c1 /= c2

isPawn :: Square -> Bool
isPawn (Piece _ Pawn) = True
isPawn _ = False

isBishop :: Square -> Bool
isBishop (Piece _ Bishop) = True
isBishop _ = False

isKnight :: Square -> Bool
isKnight (Piece _ Knight) = True
isKnight _ = False

isRook :: Square -> Bool
isRook (Piece _ Rook) = True
isRook _ = False

isQueen :: Square -> Bool
isQueen (Piece _ Queen) = True
isQueen _ = False

isKing :: Square -> Bool
isKing (Piece _ King) = True
isKing _ = False
