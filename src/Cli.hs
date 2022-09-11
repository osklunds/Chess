
-- CLI interface for playing against the computer.

module Cli
( start
)
where

import Prelude as P

import Board as B
import MoveSelection
import Game as G
import GameResult as GR

data UserAction = Move ((Int,Int),(Int,Int))
                | Undo

start :: Color -> Maybe Board -> IO ()
start playerColor maybeStartBoard = do
    let startBoard = case maybeStartBoard of
                        Nothing ->
                            defaultBoard
                        (Just board) ->
                            board

    let gs = newGameState startBoard 
    printBoard gs

    -- TODO: Playing as black doesn't work yet. Need to pass it in state.
    -- Use State monad?
    case playerColor of
        White ->
            playerTurn [gs]
        Black ->
            computerTurn (B.invert platerColor) [gs]

playerTurn :: [GameState] -> IO ()
playerTurn gss = do
  putStrLn ""
  putStrLn "Your turn!"
  playerTurnAskInput gss

playerTurnAskInput :: [GameState] -> IO ()
playerTurnAskInput gss = do
  putStr "Enter your move: "
  input <- getLine

  case parseInput input of
    Nothing -> do
        playerTurnParseError gss
    Just Undo -> do
        playerTurnUndo gss
    Just (Move move) -> do
      playerTurnValidateMove gss move

playerTurnParseError :: [GameState] -> IO ()
playerTurnParseError gss = do
    putStrLn "Illegal syntax"
    playerTurnAskInput gss

playerTurnUndo :: [GameState] -> IO ()
playerTurnUndo gss = do
    case gss of
        (_curGs:_prevGs:newGs:newRest) -> do
            putStrLn "Undoing one step"
            printBoard newGs
            playerTurnAskInput (newGs:newRest)
        [_curGs] -> do
            putStrLn "Can't undo because at start"
            playerTurnAskInput gss

playerTurnValidateMove :: [GameState] -> ((Int,Int),(Int,Int)) -> IO ()
playerTurnValidateMove gss@(curGs:_restGss) move =
    case validateMove move curGs of
        IllegalMove -> do
          putStrLn "Illegal move"
          playerTurnAskInput gss
        WouldBeInCheck -> do
          putStrLn "Doing that move would put you in check"
          playerTurnAskInput gss
        Ok -> do
          let (newGs,result) = G.applyMove move curGs
          printBoard newGs

          case result of
            Normal ->
              computerTurn (newGs:gss)
            Check -> do
              putStrLn "Computer is checked"
              computerTurn (newGs:gss)
            Checkmate ->
              putStrLn "Checkmate! You win"
            Draw ->
              putStrLn "Draw!"

computerTurn :: [GameState] -> IO ()
computerTurn gss@(curGs:_restGss) = do
  let move = moveColor 3 Black $ board curGs
  let (newGs,result) = G.applyMove move curGs
  printBoardWithMove move newGs

  case result of
    Normal ->
      playerTurn (newGs:gss)
    Check -> do
      putStrLn "You're checked"
      playerTurn (newGs:gss)
    Checkmate ->
      putStrLn "Checkmate! Computer wins"
    Draw ->
      putStrLn "Draw!"

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

-- Input ex: a4 b6
parseInput :: String -> Maybe UserAction
parseInput "undo" = Just $ Undo
parseInput [startCol,startRow,' ',destCol,destRow] = do
  startCol' <- parseCol startCol
  startRow' <- parseRow startRow
  destCol'  <- parseCol destCol
  destRow'  <- parseRow destRow

  return $ Move ((startRow',startCol'),(destRow',destCol'))
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

--------------------------------------------------------------------------------
-- Showing
--------------------------------------------------------------------------------

printBoard :: GameState -> IO ()
printBoard = putStrLn . (showBoardWithMarkers []) . board

printBoardWithMove :: ((Int,Int),(Int,Int)) -> GameState -> IO ()
printBoardWithMove (start,dest) = putStrLn .
                                  (showBoardWithMarkers [start,dest]) .
                                  board

showBoardWithMarkers :: [(Int,Int)] -> Board -> String
showBoardWithMarkers markers board =
  "  a b c d e f g h\n" ++ rows ++ "  a b c d e f g h"
  where
    rowIndexes = [0..7]
    rows       = concatMap (showRow markers board) rowIndexes

showRow :: [(Int,Int)] -> Board -> Int -> String
showRow posList board row =
  showRowIndex row ++ initialChar ++ rowString ++ showRowIndex row ++ "\n"
  where
    initialChar = betweenCharForPosition posList (row,-1)
    rowString   = P.concat [squareChar ++ betweenChar |
                            col <- [0..7],
                            let squareChar  = squareCharForPosition (row,col)
                                                                    board,
                            let betweenChar = betweenCharForPosition posList
                                                                     (row,col)]

showRowIndex :: Int -> String
showRowIndex i = show $ 8 - i

betweenCharForPosition :: [(Int,Int)] -> (Int,Int) -> String
betweenCharForPosition posList (row,col)
  | (row,col)   `elem` posList && (row,col+1) `elem` posList = "|"
  | (row,col)   `elem` posList                               = "]"
  | (row,col+1) `elem` posList                               = "["
  | otherwise                                                = " "

squareCharForPosition :: (Int,Int) -> Board -> String
squareCharForPosition pos@(row,col) board
  | square /= Empty = show square
  | even $ row+col  = " "
  | odd  $ row+col  = "■"
  where
    square = getB pos board
