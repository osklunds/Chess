
-- CLI interface for playing against the computer.

{-# LANGUAGE NamedFieldPuns #-}

module Cli
( start
)
where

import Prelude as P

import Types as T hiding (Move)
import qualified Types as T
import MoveSelection
import Game as G
import Score

data UserAction = Move T.Move
                | Undo

data State = State { gameStates :: [GameState]
                   , playerColor :: T.Color
                   }

start :: Color -> Board -> IO ()
start playerColor startBoard = do
    let gs = newGameState startBoard
    printBoard gs

    let st = State { gameStates = [gs], playerColor }

    case playerColor == getTurn startBoard of
        True ->
            playerTurn st
        False ->
            computerTurn st

playerTurn :: State -> IO ()
playerTurn st = do
    putStrLn ""
    putStrLn "Your turn!"
    playerTurnAskInput st

playerTurnAskInput :: State -> IO ()
playerTurnAskInput st = do
    putStr "Enter your move: "
    input <- getLine

    let color = playerColor st

    case parseInput input color of
        Nothing -> do
            playerTurnParseError st
        Just Undo -> do
            playerTurnUndo st
        Just (Move move) -> do
            playerTurnValidateMove st move

playerTurnParseError :: State -> IO ()
playerTurnParseError st = do
    putStrLn "Illegal syntax"
    playerTurnAskInput st

playerTurnUndo :: State -> IO ()
playerTurnUndo st = do
    let gss = gameStates st
    case gss of
        (_curGs:_prevGs:newGs:newRest) -> do
            putStrLn "Undoing one step"
            printBoard newGs
            let newGss = (newGs:newRest)
            let newSt = st { gameStates = newGss }
            playerTurnAskInput newSt
        [_curGs] -> do
            putStrLn "Can't undo because at start"
            playerTurnAskInput st

playerTurnValidateMove :: State -> T.Move -> IO ()
playerTurnValidateMove st move = do
    let gss@(curGs:_restGss) = gameStates st
    case validateMove move curGs of
        IllegalMove -> do
          putStrLn "Illegal move"
          playerTurnAskInput st
        WouldBeInCheck -> do
          putStrLn "Doing that move would put you in check"
          playerTurnAskInput st
        Ok -> do
          let (newGs,result) = G.applyMove move curGs
          printBoard newGs

          case result of
            Normal False ->
              computerTurn $ st { gameStates = (newGs:gss) }
            Normal True -> do
              putStrLn "Computer is checked"
              computerTurn $ st { gameStates = (newGs:gss) }
            Checkmate ->
              putStrLn "Checkmate! You win"
            Draw ->
              putStrLn "Draw!"

computerTurn :: State -> IO ()
computerTurn st = do
    let gss@(curGs:_restGss) = gameStates st
    let computerColor = T.invert $ playerColor st
    let move = selectMove 4 $ board curGs
    let (newGs,result) = G.applyMove move curGs
    putStrLn ""
    printBoardWithMove move newGs

    case result of
        Normal False ->
            playerTurn $ st { gameStates = (newGs:gss) }
        Normal True -> do
            putStrLn "You're checked"
            playerTurn $ st { gameStates = (newGs:gss) }
        Checkmate ->
            putStrLn "Checkmate! Computer wins"
        Draw ->
            putStrLn "Draw!"

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

-- Input ex: a4 b6
parseInput :: String -> Color -> Maybe UserAction
-- Undo
parseInput "undo" _c = Just $ Undo

-- NormalMove
parseInput [startCol,startRow,' ',destCol,destRow] _c = do
    startCol' <- parseCol startCol
    startRow' <- parseRow startRow
    let start = (Pos startRow' startCol')
    destCol'  <- parseCol destCol
    destRow'  <- parseRow destRow
    let dest = (Pos destRow' destCol')
    return $ Move $ NormalMove start dest

-- Promote
parseInput [startCol,startRow,' ',destCol,destRow,kind] _c = do
    startCol' <- parseCol startCol
    startRow' <- parseRow startRow
    let start = (Pos startRow' startCol')
    destCol'  <- parseCol destCol
    destRow'  <- parseRow destRow
    let dest = (Pos destRow' destCol')
    kind' <- parseKind kind
    return $ Move $ Promote start dest kind'
  -- TODO: Make common helper for normal and promote

-- Castle
parseInput "king" c = return $ Move $ Castle c KingSide
parseInput "queen" c = return $ Move $ Castle c QueenSide

-- Bad input
parseInput _input _c= Nothing

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

parseKind :: Char -> Maybe Kind
parseKind 'q' = Just Queen
parseKind 'r' = Just Rook
parseKind 'b' = Just Bishop
parseKind 'k' = Just Knight


--------------------------------------------------------------------------------
-- Showing
--------------------------------------------------------------------------------

printBoard :: GameState -> IO ()
printBoard = putStrLn . (showBoardWithMarkers []) . board

printBoardWithMove :: T.Move -> GameState -> IO ()
printBoardWithMove move = putStrLn . (showBoardWithMarkers markers) . board
    where
        markersAsPos = case move of
                    (NormalMove src dst) -> [src, dst]
                    (Promote src dst _kind) -> [src, dst]
                    (Castle Black _) -> [(Pos 0 4)]
                    (Castle White _) -> [(Pos 7 4)]
        markers = [(r,c) | (Pos r c) <- markersAsPos]

showBoardWithMarkers :: [(Int,Int)] -> Board -> String
showBoardWithMarkers markers board =
  "  " ++ show (getCastleState Black board) ++ "\n" ++
  "  a b c d e f g h\n" ++
  rows ++
  "  a b c d e f g h\n" ++
  "  " ++ show (getCastleState White board)
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
squareCharForPosition (row,col) board
  | square /= Empty = show square
  | even $ row+col  = " "
  | odd  $ row+col  = "■"
  where
    square = getB (Pos row col) board
