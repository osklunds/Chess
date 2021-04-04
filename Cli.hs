
module Cli
( main
)
where

import Prelude as P

import Board
import Moves
import MoveSelection


-- Player hard coded as White, computer as Black

main :: IO ()
main = do
  let board = defaultBoard
  putStrLn $ showBoard board
  beginTurn playerTurn board

beginTurn :: (Board -> IO ()) -> Board -> IO ()
beginTurn nextState board = do
  putStrLn ""
  nextState board

playerTurn :: Board -> IO ()
playerTurn board = do
  putStrLn "Your turn!"
  playerTurn' board

playerTurn' :: Board -> IO ()
playerTurn' board = do
  putStr "Enter your move: "
  input <- getLine

  case parseInput input of
    Nothing -> do
      putStrLn "Illegal syntax"
      playerTurn' board
    Just move -> do
      let allMoves = movesForColor White board

      case move `elem` allMoves of
        True -> do
          let newBoard = applyMove move board
          putStrLn $ showBoard newBoard
          beginTurn computerTurn newBoard
        False -> do
          putStrLn "Illegal move"
          playerTurn' board

-- Input ex: a4 b6
parseInput :: String -> Maybe ((Int,Int),(Int,Int))
parseInput [startCol,startRow,' ',destCol,destRow] = do
  startCol' <- parseCol startCol
  startRow' <- parseRow startRow
  destCol'  <- parseCol destCol
  destRow'  <- parseRow destRow

  return ((startRow',startCol'),(destRow',destCol'))
parseInput _ = Nothing

parseCol :: Char -> Maybe Int
parseCol 'a' = Just 0
parseCol 'b' = Just 1
parseCol 'c' = Just 2
parseCol 'd' = Just 3
parseCol 'e' = Just 4
parseCol 'f' = Just 5
parseCol 'g' = Just 6
parseCol 'h' = Just 7
parseCol _   = Nothing

parseRow :: Char -> Maybe Int
parseRow '8' = Just 0
parseRow '7' = Just 1
parseRow '6' = Just 2
parseRow '5' = Just 3
parseRow '4' = Just 4
parseRow '3' = Just 5
parseRow '2' = Just 6
parseRow '1' = Just 7
parseRow _   = Nothing

computerTurn :: Board -> IO ()
computerTurn board = do
  let move = moveColor 4 Black board
  let newBoard = applyMove move board

  putStrLn $ showBoardWithMove move newBoard

  beginTurn playerTurn newBoard

showBoardWithMove :: ((Int,Int),(Int,Int)) -> Board -> String
showBoardWithMove (start,dest) board =
  "  a b c d e f g h\n" ++ rowsWithSquares ++ "  a b c d e f g h"
  where
    rowIndexes      = [0..7]
    rowsWithSquares = (concatMap (showRow [start,dest] board) rowIndexes)

showRow :: [(Int,Int)] -> Board -> Int -> String
showRow positions board row =
  showRowIndex row ++ initialChar ++ rowString ++ showRowIndex row ++ "\n"
  where
    initialChar = getCharForPosition positions (row,-1)
    rowString   = P.concat [show sq ++ char |
                            col <- [0..7],
                            let sq   = get (row,col) board,
                            let char = getCharForPosition positions
                                                          (row,col)]

showRowIndex :: Int -> String
showRowIndex i = show $ 8 - i

getCharForPosition :: [(Int,Int)] -> (Int,Int) -> String
getCharForPosition positions (row,col)
  | (row,col)   `elem` positions &&
    (row,col+1) `elem` positions = "|"
  | (row,col)   `elem` positions = "]"
  | (row,col+1) `elem` positions = "["
  | otherwise                    = " "
