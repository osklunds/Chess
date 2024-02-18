
{-# LANGUAGE TemplateHaskell #-}

module MoveSelection.Tests where

import Test.QuickCheck

import Types
import Moves
import MoveSelection


--------------------------------------------------------------------------------
-- Fixed
--------------------------------------------------------------------------------

prop_capture :: Property
prop_capture = conjoin (map verifyCapturesPiece nonKingKinds)

verifyCapturesPiece :: Kind -> Property
verifyCapturesPiece kind = verifyMakesMove expMove board'
  where
    src = Pos 5 5
    dst = Pos 6 4
    expMove = NormalMove src dst

    board  = read  "  U       M     U  \n\
                   \  0 1 2 3 4 5 6 7  \n\
                   \0                 0\n\
                   \1 ♔               1\n\
                   \2                 2\n\
                   \3                 3\n\
                   \4                 4\n\
                   \5           ♟ ♚   5\n\
                   \6        [ ]      6\n\
                   \7                 7\n\
                   \  0 1 2 3 4 5 6 7  \n\
                   \  U       M     M"
    piece  = Piece White kind
    board' = setB dst piece board

prop_escape :: Bool
-- TODO: Use Int5 etc for depths instead
prop_escape = and [verifyEscapesFromThreat d k | d <- depths, k <- nonKingKinds]

verifyEscapesFromThreat :: Int -> Kind -> Bool
verifyEscapesFromThreat depth kind = all (\pos -> isEmpty $ getB pos board'') 
                                         threatenedPosList
  where
    curPos            = Pos 5 4
    threatenedPosList = [curPos, Pos 5 2, Pos 5 4, Pos 0 0, Pos 0 1, Pos 1 1,
                                 Pos 2 1, Pos 2 0]
    piece             = Piece Black kind

    board   = read  "  M       M     M  \n\
                    \  0 1 2 3 4 5 6 7  \n\
                    \0               ♚ 0\n\
                    \1 ♔               1\n\
                    \2                 2\n\
                    \3                 3\n\
                    \4                 4\n\
                    \5        [ ]      5\n\
                    \6       ♙         6\n\
                    \7                 7\n\
                    \  0 1 2 3 4 5 6 7  \n\
                    \  M       M     M"
    board'  = setB curPos piece board
    move    = makeMove depth board'
    board'' = applyMove move board'

prop_checkmate :: Property
prop_checkmate = verifyMakesOneOfMoves expMoves board
  where
    expMoves = [NormalMove (Pos 7 1) (Pos 7 0), NormalMove (Pos 6 1) (Pos 6 0)]
    -- The board looks like this to reduce the number of moves to speed up
    board = read  "  U       U     U  \n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \0 ♔   ♝           0\n\
                  \1     ♟           1\n\
                  \2     ♟           2\n\
                  \3     ♟           3\n\
                  \4     ♟         ♚ 4\n\
                  \5     ♟           5\n\
                  \6   ♜ ♟           6\n\
                  \7   ♜ ♞           7\n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \  U       U     U"

prop_checkmateByMovingAwayPiece :: Property
prop_checkmateByMovingAwayPiece = verifyMakesOneOfMoves expMoves board
  where
    expMoves = [NormalMove (Pos 3 0) (Pos 1 2), NormalMove (Pos 3 0) (Pos 5 2)]
    -- The board looks like this to reduce the number of moves to speed up
    board = read  "  U       U     U  \n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \0 ♔     ♞         0\n\
                  \1       ♟         1\n\
                  \2       ♟         2\n\
                  \3 ♝     ♟         3\n\
                  \4       ♟       ♚ 4\n\
                  \5       ♟         5\n\
                  \6       ♟         6\n\
                  \7 ♜ ♜   ♞         7\n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \  U       U     U"

prop_escapeFromCheckEvenIfCanCheckmate :: Bool
prop_escapeFromCheckEvenIfCanCheckmate =
  and [isEmpty $ getB (Pos 4 4) (nextBoard d) | d <- depths]
  where
    board = read  "  U       U     U  \n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \0 ♔               0\n\
                  \1                 1\n\
                  \2           ♘     2\n\
                  \3                 3\n\
                  \4         ♚       4\n\
                  \5                 5\n\
                  \6     ♜           6\n\
                  \7   ♜             7\n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \  U       U     U"
    nextBoard d = applyMove (makeMove d board) board

prop_doStalemateIfLosing :: Property
prop_doStalemateIfLosing = verifyMakesMove expMove board
  where
    board = read  "  U       U     U  \n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \0       ♚         0\n\
                  \1                 1\n\
                  \2 ♙   ♕ ♙         2\n\
                  \3   ♙             3\n\
                  \4                 4\n\
                  \5               ♙ 5\n\
                  \6           ♛ ♙   6\n\
                  \7               ♔ 7\n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \  U       U     U"
    expMove = NormalMove (Pos 6 5) (Pos 7 6)
-- The above board comes from
-- https://www.chessgames.com/perl/chessgame?gid=1255706
-- James Adams Congdon vs Eugene Delmar
-- Right now, Black is in a losing position. There's not much hope for Black
-- to win, but there's a possibility to create a draw and thus at least avoid
-- losing.

prop_promote :: Property
prop_promote = verifyMakesMove expMove board
    where
        board = read  "  U       U     U  \n\
                      \  0 1 2 3 4 5 6 7  \n\
                      \0               ♚ 0\n\
                      \1         ♙       1\n\
                      \2                 2\n\
                      \3                 3\n\
                      \4                 4\n\
                      \5     ♔           5\n\
                      \6         ♟       6\n\
                      \7                 7\n\
                      \  0 1 2 3 4 5 6 7  \n\
                      \  U       U     U"
        expMove = Promote (Pos 6 4) (Pos 7 4) Queen

prop_promoteToKnight :: Property
prop_promoteToKnight = verifyMakesMove expMove board
    where
        board = read  "  U       U     U  \n\
                      \  0 1 2 3 4 5 6 7  \n\
                      \0               ♚ 0\n\
                      \1                 1\n\
                      \2                 2\n\
                      \3                 3\n\
                      \4                 4\n\
                      \5   ♙ ♘ ♙   ♝     5\n\
                      \6   ♙ ♔ ♙ ♟       6\n\
                      \7   ♘ ♘           7\n\
                      \  0 1 2 3 4 5 6 7  \n\
                      \  U       U     U"
        expMove = Promote (Pos 6 4) (Pos 7 4) Knight

prop_doNotPromote1 :: Property
prop_doNotPromote1 = verifyMakesMove expMove board
    where
        board = read  "  U       U     U  \n\
                      \  0 1 2 3 4 5 6 7  \n\
                      \0   ♜           ♚ 0\n\
                      \1     ♜           1\n\
                      \2                 2\n\
                      \3                 3\n\
                      \4                 4\n\
                      \5 ♔               5\n\
                      \6         ♟       6\n\
                      \7                 7\n\
                      \  0 1 2 3 4 5 6 7  \n\
                      \  U       U     U"
        expMove = NormalMove (Pos 1 2) (Pos 1 0)

prop_doNotPromote2 :: Property
prop_doNotPromote2 = verifyMakesMove expMove board
    where
        board = read  "  U       U     U  \n\
                      \  0 1 2 3 4 5 6 7  \n\
                      \0               ♚ 0\n\
                      \1       ♟         1\n\
                      \2     ♕           2\n\
                      \3                 3\n\
                      \4                 4\n\
                      \5 ♔               5\n\
                      \6             ♟   6\n\
                      \7                 7\n\
                      \  0 1 2 3 4 5 6 7  \n\
                      \  U       U     U"
        expMove = NormalMove (Pos 1 3) (Pos 2 2)

prop_castling :: Property
prop_castling = verifyMakesMove expMove board
    where
        board = castleToWinBoard
        expMove = Castle Black QueenSide

castleToWinBoard :: Board
castleToWinBoard = read  "  U       U     U  \n\
                         \  0 1 2 3 4 5 6 7  \n\
                         \0 ♜       ♚   ♞   0\n\
                         \1                 1\n\
                         \2   ♙ ♙ ♔ ♙       2\n\
                         \3 ♘   ♙   ♙       3\n\
                         \4         ♙       4\n\
                         \5 ♗   ♙           5\n\
                         \6 ♟   ♘         ♞ 6\n\
                         \7     ♖   ♘       7\n\
                         \  0 1 2 3 4 5 6 7  \n\
                         \  M       M     M"

prop_doNotCastleIfThreatened :: Bool
prop_doNotCastleIfThreatened = verifyDoesNotCastle board
    where
        board = setB (Pos 4 7) (Piece White Bishop) castleToWinBoard

prop_doNotCastleIfHasMoved :: Bool
prop_doNotCastleIfHasMoved = verifyDoesNotCastle board
    where
        newCastleState = CastleState { leftRook = Moved, king = Unmoved, rightRook = Moved }
        board = setCastleState Black newCastleState castleToWinBoard

--------------------------------------------------------------------------------
-- Promote defer bug
--------------------------------------------------------------------------------

-- For the board below, the move selection algorithm has "historically" not done
-- the obvious promote move and insted defered it forever, when depth > 2. The bug
-- has not been triggered when using MiniMax, but only when using AlphaBeta. However, if
-- the order of generated states in MiniMax is reversed, then the bug is triggered
-- also for MiniMax. So make sure there's good coverage on this bug.
prop_deferPromoteBug1 :: Property
prop_deferPromoteBug1 = verifyMakesMove expMove board
    where
        board = read  "  U       U     U  \n\
                      \  0 1 2 3 4 5 6 7  \n\
                      \0                 0\n\
                      \1       ♚         1\n\
                      \2                 2\n\
                      \3                 3\n\
                      \4                 4\n\
                      \5     ♔           5\n\
                      \6         ♟       6\n\
                      \7                 7\n\
                      \  0 1 2 3 4 5 6 7  \n\
                      \  U       U     U"
        expMove = Promote (Pos 6 4) (Pos 7 4) Queen

prop_deferPromoteBug2 :: Property
prop_deferPromoteBug2 = verifyMakesMove expMove board
    where
        board = read  "  U       U     U  \n\
                      \  0 1 2 3 4 5 6 7  \n\
                      \0                 0\n\
                      \1                 1\n\
                      \2             ♔   2\n\
                      \3                 3\n\
                      \4         ♚       4\n\
                      \5                 5\n\
                      \6 ♟               6\n\
                      \7                 7\n\
                      \  0 1 2 3 4 5 6 7  \n\
                      \  U       U     U"
        expMove = Promote (Pos 6 0) (Pos 7 0) Queen

prop_deferPromoteBug3 :: Property
prop_deferPromoteBug3 = verifyMakesMove expMove board
    where
        board = read  "  U       U     U  \n\
                      \  0 1 2 3 4 5 6 7  \n\
                      \0                 0\n\
                      \1                 1\n\
                      \2             ♔   2\n\
                      \3                 3\n\
                      \4                 4\n\
                      \5                 5\n\
                      \6 ♟               6\n\
                      \7             ♚   7\n\
                      \  0 1 2 3 4 5 6 7  \n\
                      \  U       U     U"
        expMove = Promote (Pos 6 0) (Pos 7 0) Queen

prop_deferPromoteBug4 :: Property
prop_deferPromoteBug4 = verifyMakesMove expMove board
    where
        board = read  "  U       U     U  \n\
                      \  0 1 2 3 4 5 6 7  \n\
                      \0                 0\n\
                      \1                 1\n\
                      \2             ♔   2\n\
                      \3                 3\n\
                      \4                 4\n\
                      \5                 5\n\
                      \6       ♟         6\n\
                      \7   ♚             7\n\
                      \  0 1 2 3 4 5 6 7  \n\
                      \  U       U     U"
        expMove = Promote (Pos 6 3) (Pos 7 3) Queen

prop_deferPromoteBugArbitrary :: Int -> Pos -> Pos -> Property
prop_deferPromoteBugArbitrary pawnCol' blackKingPos whiteKingPos =
    condition ==> (verifyMakesMove expMove withPawn)
    where
        -- Positions
        pawnCol = pawnCol' `mod` 8
        pawnStart = Pos 6 pawnCol
        pawnEnd = Pos 7 pawnCol

        -- Conditions
        notTooClose p1 p2 = posDist p1 p2 >= 2
        
        condition = notTooClose blackKingPos whiteKingPos &&
                    notTooClose blackKingPos pawnStart &&
                    notTooClose blackKingPos pawnEnd &&

                    notTooClose whiteKingPos blackKingPos &&
                    notTooClose whiteKingPos pawnStart &&
                    notTooClose whiteKingPos pawnEnd
    
        emptyBoard = read  "  U       U     U  \n\
                           \  0 1 2 3 4 5 6 7  \n\
                           \0                 0\n\
                           \1                 1\n\
                           \2                 2\n\
                           \3                 3\n\
                           \4                 4\n\
                           \5                 5\n\
                           \6                 6\n\
                           \7                 7\n\
                           \  0 1 2 3 4 5 6 7  \n\
                           \  U       U     U"
        withBlackKing = setB blackKingPos (Piece Black King) emptyBoard
        withWhiteKing = setB whiteKingPos (Piece White King) withBlackKing
        withPawn = setB pawnStart (Piece Black Pawn) withWhiteKing
        expMove = Promote pawnStart pawnEnd Queen
        

posDist :: Pos -> Pos -> Int
posDist (Pos r1 c1) (Pos r2 c2) = max (abs (r1 - r2)) (abs (c1 - c2))
    
-- TODO: Test move that needs two steps ahead thinking

-- TODO: Use a real game as inspiration. For tricky situations, test that the
-- computer does the correct thing


--------------------------------------------------------------------------------
-- Arbitrary
--------------------------------------------------------------------------------

prop_legalMove :: Board -> Property
prop_legalMove board = not (null legalMoves) ==> result
    where
        legalMoves = movesFun Black board
        -- TODO: Test for more depths when the algorithm is faster
        result = and [makeMove d board `elem` legalMoves | d <- [1..1]]

-- TODO: escape from check

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

makeMove :: Int -> Board -> Move
makeMove depth board = moveColor depth Black board

makeMoveWhite :: Int -> Board -> Move
makeMoveWhite depth board = moveColor depth White board

-- TODO: make depth a parameter to more functions
depths :: [Int]
depths = [2..3]

nonKingKinds :: [Kind]
nonKingKinds = [Queen, Rook, Bishop, Knight, Pawn]

verifyMakesMove :: Move -> Board -> Property
verifyMakesMove expMove = verifyMakesOneOfMoves [expMove]

verifyMakesOneOfMoves :: [Move] -> Board -> Property
verifyMakesOneOfMoves expMoves board = conjoin props
    where
        props = [verifyMakesOneOfMovesAtDepth expMoves board d | d <- depths]

verifyMakesOneOfMovesAtDepth :: [Move] -> Board -> Int -> Property
verifyMakesOneOfMovesAtDepth expMoves board depth =
    counterexample errorString result
    where
        result = move `elem` expMoves
        move = makeMove depth board 
        errorString = show board ++ "\n" ++
                      "Expected moves: " ++ show expMoves ++ "\n\n" ++
                      "Actual move: " ++ show move ++ "\n\n" ++
                      "Depth: " ++ show depth

verifyDoesNotCastle :: Board -> Bool
verifyDoesNotCastle board = all pred depths
    where
        -- I used to check "does not make move", but that is risky with castle since
        -- if color by mistake is flipped, it will return true for the wrong reason.
        pred depth = not $ isCastle $ makeMove depth board


return []
runTests = $quickCheckAll
