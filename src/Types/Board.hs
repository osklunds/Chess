
{-# LANGUAGE LambdaCase #-}

module Types.Board
( Board

, getB
, setB
, foldB
, concatB
, mapB
, anyB

, applyMove
, defaultBoard
, generateBoard
, sampleBoard
, homeRow
, numKings
)
where

import Control.Monad
import Control.Exception
import Data.Char
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Types.Pos
import Types.Square
import Types.Move

--------------------------------------------------------------------------------
-- Data types
--------------------------------------------------------------------------------

newtype Board = Board [[Square]]
              deriving (Eq, Ord)

--------------------------------------------------------------------------------
-- Show
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Read
--------------------------------------------------------------------------------

instance Read Board where
  readsPrec _ str = [(fromString str, "")]

fromString :: String -> Board
fromString board = Board $ map f rows''
    where
        rows   = lines board
        rows'  = tail rows
        rows'' = init rows'

        f row  = [read [c] | (c,i) <- rowWithIndexes, not (isDigit c), even i]
            where
                rowWithIndexes = zip row [0..]

--------------------------------------------------------------------------------
-- Arbitrary
--------------------------------------------------------------------------------

instance Arbitrary Board where
    arbitrary = arbitraryBoard

arbitraryBoard :: Gen Board
arbitraryBoard = do
    rows <- replicateM 8 $ replicateM 8 arbitrary

    let board = Board rows
    let noKings = mapB (\sq -> if isKing sq then Empty else sq) board
    
    castle <- oneIn 4
    afterCastle <- case castle of
                        True ->
                            adjustForCastle noKings
                        False ->
                            return noKings

    blackKing <- ensureKing Black afterCastle
    whiteKing <- ensureKing White blackKing

    return $ checkedBoard whiteKing

ensureKing :: Color -> Board -> Gen Board
ensureKing color board = do
    let king = Piece color King
    case anyB (== king) board of
        True ->
            return board
        False -> do
            pos <- arbitrary
            case isKing (getB pos board) of
                True ->
                    ensureKing color board
                False ->
                    return $ setB pos king board

adjustForCastle :: Board -> Gen Board
adjustForCastle board = do
    let foldlFun b (pos,sq) = maybeChangeTo pos sq b
    let blackCastleRow = (castleRow Black 0)
    afterBlack <- foldM foldlFun board blackCastleRow
    let whiteCastleRow = (castleRow White 7)
    foldM foldlFun afterBlack whiteCastleRow

maybeChangeTo :: Pos -> Square -> Board -> Gen Board
maybeChangeTo pos square board = do
    change <- fmap not $ oneIn 4
    return $ case change of
                True ->
                    setB pos square board
                False ->
                    board

castleRow :: Color -> Int -> [(Pos,Square)]
castleRow color row = zip posList pieces
    where
        posList = [Pos row col | col <- [0..7]]
        pieces = [Piece color Rook,
                  Empty,
                  Empty,
                  Empty,
                  Piece color King,
                  Empty,
                  Empty,
                  Piece color Rook]

oneIn :: Int -> Gen Bool
oneIn n = do
    n' <- elements [1..n]
    return $ n == n'

instance Arbitrary Side where
    arbitrary = oneof $ map return [KingSide, QueenSide]

--------------------------------------------------------------------------------
-- Board operations
--------------------------------------------------------------------------------

getB :: Pos -> Board -> Square
getB (Pos row col) (Board board) = (board !! row) !! col

setB :: Pos -> Square -> Board -> Board
setB pos sq b = checkedBoard newBoard
    where
        newBoard = setB' pos sq b

-- Same as setB, but no consisteny check
setB' :: Pos -> Square -> Board -> Board
setB' (Pos rowIdx colIdx) sq (Board oldBoard) = Board $ newBoard
    where
        oldRow = oldBoard !! rowIdx
        newRow = replaceAt colIdx sq oldRow
        newBoard = replaceAt rowIdx newRow oldBoard

checkedBoard :: Board -> Board
checkedBoard b = assert (checkBoard b) b

checkBoard :: Board -> Bool
checkBoard b = checkNumKings b

checkNumKings :: Board -> Bool
checkNumKings b = numBlack <= 1 && numWhite <= 1
    where
        (numBlack,numWhite) = numKings b

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs = prefix ++ [x] ++ suffix
  where
    (prefix,(_:suffix)) = splitAt i xs

foldB :: (a -> Square -> a) -> a -> Board -> a
foldB f v board = foldl f v $ concatB board

concatB :: Board -> [Square]
concatB (Board rows) = concat rows

mapB :: (Square -> Square) -> Board -> Board
mapB f (Board rows) = checkedBoard $ Board $ map (\row -> map f row) rows

anyB :: (Square -> Bool) -> Board -> Bool
anyB f = foldB (\acc square -> acc || f square) False

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

applyMove :: Move -> Board -> Board
applyMove move board = checkedBoard newBoard
    where
        newBoard = case move of
                    (NormalMove src dst) -> applyNormalMove src dst board
                    (Promote p kind)     -> applyPromote p kind board
                    (Castle color side)  -> applyCastle color side board

applyNormalMove :: Pos -> Pos -> Board -> Board
applyNormalMove src dst b = assert (not $ isEmpty atSrc)
                                   (setB' dst atSrc $ setB' src Empty b)
    where
        atSrc = getB src b

applyPromote :: Pos -> Kind -> Board -> Board
applyPromote p kind b = assert condition setB' p newAtP b
    where
        -- TODO: assert row+color, kind
        atP    = getB p b
        newAtP = Piece (color atP) kind

        condition = pawnAtP && notToPawnOrKing && pAtTopOrBottom
        pawnAtP = isPawn atP
        notToPawnOrKing = kind /= Pawn && kind /= King
        pAtTopOrBottom = row == 1 && color atP == White ||
                         row == 6 && color atP == Black
        (Pos row _col) = p

applyCastle :: Color -> Side -> Board -> Board
applyCastle color side board = board'
    where
        row = case color of
                White -> 7
                Black -> 0
        kingCol = 4
        (rookCol,newRookCol,newKingCol) = case side of
                                            KingSide -> (7,5,6)
                                            QueenSide -> (0,3,2)
        board' = setB' (Pos row kingCol)    Empty $
                 setB' (Pos row newKingCol) (Piece color King) $
                 setB' (Pos row rookCol)    Empty $
                 setB' (Pos row newRookCol) (Piece color Rook) board

defaultBoard :: Board
defaultBoard = Board $ [map (\kind -> Piece Black kind) defaultRow] ++
                       [replicate 8 $ Piece Black Pawn] ++
                       (replicate 4 $ replicate 8 $ Empty) ++
                       [replicate 8 $ Piece White Pawn] ++
                       [map (\kind -> Piece White kind) defaultRow]

defaultRow = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]


generateBoard :: IO Board
generateBoard = generate arbitrary

sampleBoard :: IO ()
sampleBoard = sample (arbitrary :: Gen Board)

homeRow :: Color -> Int
homeRow White = 7
homeRow Black = 0

numKings :: Board -> (Int,Int)
numKings b = foldB f (0,0) b
    where
        f (numBlack, numWhite) sq =
                case sq of
                    (Piece Black King) -> (numBlack + 1, numWhite)
                    (Piece White King) -> (numBlack, numWhite + 1)
                    _pos               -> (numBlack, numWhite)