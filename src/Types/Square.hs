
module Types.Square
( Square(..)
, Color(..)
, Kind(..)

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

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

--------------------------------------------------------------------------------
-- Data types
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Show
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Arbitrary
--------------------------------------------------------------------------------

instance Arbitrary Square where
  arbitrary = oneof [return Empty,
                     do
                      color <- arbitrary
                      kind  <- arbitrary
                      return $ Piece color kind
                    ]

instance Arbitrary Color where
  arbitrary = elements [Black, White]


instance Arbitrary Kind where
  arbitrary = elements [Pawn, Bishop, Knight, Rook, Queen, King]

--------------------------------------------------------------------------------
-- Functions
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
