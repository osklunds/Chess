
{-# LANGUAGE TemplateHaskell #-}

module MoveSelection.Tests where

import Test.QuickCheck
import Control.Exception

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
                   \  U       M     M\n\
                   \[Black]"
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
                    \  M       M     M\n\
                    \[Black]"
    board'  = setB curPos piece board
    move    = selectMove depth board'
    board'' = applyMove move board'


-- TODO: Without the pawn, the test fails
prop_captureKing :: Property
prop_captureKing = verifyMakesMove expMove board
  where
    expMove = NormalMove (Pos 0 2) (Pos 0 0)
    board = read  "  U       U     U  \n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \0 ♔   ♜           0\n\
                  \1                 1\n\
                  \2                 2\n\
                  \3         ♙       3\n\
                  \4               ♚ 4\n\
                  \5                 5\n\
                  \6                 6\n\
                  \7   ♜             7\n\
                  \  0 1 2 3 4 5 6 7  \n\
                  \  U       U     U\n\
                  \[Black]"

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
                  \  U       U     U\n\
                  \[Black]"

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
                  \  U       U     U\n\
                  \[Black]"

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
                  \  U       U     U\n\
                  \[Black]"
    nextBoard d = applyMove (selectMove d board) board

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
                  \  U       U     U\n\
                  \[Black]"
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
                      \  U       U     U\n\
                      \[Black]"
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
                      \  U       U     U\n\
                      \[Black]"
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
                      \  U       U     U\n\
                      \[Black]"
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
                      \  U       U     U\n\
                      \[Black]"
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
                         \  M       M     M\n\
                         \[Black]"

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
                      \  U       U     U\n\
                      \[Black]"
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
                      \  U       U     U\n\
                      \[Black]"
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
                      \  U       U     U\n\
                      \[Black]"
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
                      \  U       U     U\n\
                      \[Black]"
        expMove = Promote (Pos 6 3) (Pos 7 3) Queen

-- TOOD: This test worked in the initial commit about solving the promote bug.
-- I must have broken something since then.
-- Actually no... just changing the expected value causes actual value to change... what??
-- Could it be that if king is moved, in the next turn, you can make it a queen?
-- Yes, 2 deep OK. 3 deep moves king because thinks can make queen, but 4 deep,
-- sees that white will capture the queen, so 4 is OK.
prop_deferPromoteBug5 :: Property
prop_deferPromoteBug5 = conjoin [verifyMakesMove' expMoveSmart board 2,
                                 verifyMakesMove' expMoveDumb  board 3,
                                 verifyMakesMove' expMoveSmart board 4,
                                 verifyMakesMove' expMoveSmart board 5,
                                 verifyMakesMove' expMoveSmart board 6,
                                 verifyMakesMove' expMoveSmart board 7
                                ]
    -- If thinking 3 steps ahead, Black sees that now, if I promote to a queen,
    -- there will be a stalemate. So if I first move my king, then I can promote
    -- to queen in the next move. But what 3 steps ahead misses is that then
    -- White can capture the just-promoted-queen. If thinking 4+ steps ahead,
    -- Black does not make that mistake.
    where
        board = read  "  U       U     U  \n\
                      \  0 1 2 3 4 5 6 7  \n\
                      \0                 0\n\
                      \1                 1\n\
                      \2                 2\n\
                      \3                 3\n\
                      \4               ♚ 4\n\
                      \5                 5\n\
                      \6           ♟   ♔ 6\n\
                      \7                 7\n\
                      \  0 1 2 3 4 5 6 7  \n\
                      \  U       U     U\n\
                      \[Black]"
        expMoveSmart = Promote (Pos 6 5) (Pos 7 5) Rook
        expMoveDumb = NormalMove (Pos 4 7) (Pos 4 6)

prop_deferPromoteBug6 :: Property
prop_deferPromoteBug6 = verifyMakesMove expMove board
    where
        board = read  "  U       U     U  \n\
                      \  0 1 2 3 4 5 6 7  \n\
                      \0                 0\n\
                      \1                 1\n\
                      \2                 2\n\
                      \3                 3\n\
                      \4                 4\n\
                      \5   ♚             5\n\
                      \6       ♟         6\n\
                      \7 ♔               7\n\
                      \  0 1 2 3 4 5 6 7  \n\
                      \  U       U     U\n\
                      \[Black]"
        expMove = Promote (Pos 6 3) (Pos 7 3) Rook

-- Need to disable because generates so many cases where it's actually
-- good to defer the promote
-- prop_deferPromoteBugArbitrary :: Int -> Pos -> Pos -> Property
-- prop_deferPromoteBugArbitrary pawnCol' blackKingPos whiteKingPos =
--     condition ==> (verifyMakesMove expMove board4)
--     where
--         -- Positions
--         pawnCol = pawnCol' `mod` 8
--         pawnStart = Pos 6 pawnCol
--         pawnEnd = Pos 7 pawnCol

--         -- Conditions
--         notTooClose p1 p2 = posDist p1 p2 >= 2
        
--         condition = notTooClose blackKingPos whiteKingPos &&
--                     notTooClose blackKingPos pawnStart &&
--                     notTooClose blackKingPos pawnEnd &&

--                     notTooClose whiteKingPos blackKingPos &&
--                     notTooClose whiteKingPos pawnStart &&
--                     notTooClose whiteKingPos pawnEnd &&

--                     -- Why not? See prop_deferPromoteBug5
--                     not (whiteKingPos `elem` [Pos 6 6, Pos 6 7] &&
--                          blackKingPos `elem` [Pos 4 6, Pos 4 7]) &&

--                     not (whiteKingPos `elem` [Pos 6 0, Pos 6 1] &&
--                          blackKingPos `elem` [Pos 4 0, Pos 4 1]) &&
                    
--                     -- Why not? See prop_deferPromoteBug6
--                     not (whiteKingPos == (Pos 7 0) &&
--                          blackKingPos `elem` [Pos 5 0, Pos 5 1]) &&

--                     not (whiteKingPos == (Pos 7 7) &&
--                          blackKingPos `elem` [Pos 5 6, Pos 5 7]) &&

--                     -- Why not? See prop_deferPromoteBug7
--                     not (rowOf whiteKingPos == 7 &&
--                          rowOf blackKingPos == 5 &&
--                          colOf whiteKingPos == colOf blackKingPos)
    
--         board1 = read  "  U       U     U  \n\
--                        \  0 1 2 3 4 5 6 7  \n\
--                        \0                 0\n\
--                        \1                 1\n\
--                        \2                 2\n\
--                        \3                 3\n\
--                        \4                 4\n\
--                        \5                 5\n\
--                        \6                 6\n\
--                        \7                 7\n\
--                        \  0 1 2 3 4 5 6 7  \n\
--                        \  U       U     U\n\
--                        \[Black]"
--         board2 = setB blackKingPos (Piece Black King) board1
--         board3 = setB whiteKingPos (Piece White King) board2
--         board4 = setB pawnStart (Piece Black Pawn) board3
--         expMove = Promote pawnStart pawnEnd Queen

prop_deferPromoteBug7 :: Property
prop_deferPromoteBug7 = verifyMakesOneOfMoves [move1, move2] board
    where
        board = read  "  U       U     U  \n\
                      \  0 1 2 3 4 5 6 7  \n\
                      \0                 0\n\
                      \1                 1\n\
                      \2                 2\n\
                      \3                 3\n\
                      \4                 4\n\
                      \5     ♚           5\n\
                      \6 ♟               6\n\
                      \7     ♔           7\n\
                      \  0 1 2 3 4 5 6 7  \n\
                      \  U       U     U\n\
                      \[Black]"
        move1 = Promote (Pos 6 0) (Pos 7 0) Queen
        move2 = Promote (Pos 6 0) (Pos 7 0) Rook

-- Here the correct move is actually to defer the promote
prop_deferPromoteBug8 :: Property
prop_deferPromoteBug8 = assert (board2 == board2') $
                        assert (moves == [onlyMove]) $
                        assert (board3 == board3') $
                        verifyMakesMove' move1 board1 3 .&&.
                        verifyMakesOneOfMoves' moves3 board3 3
    where
        board1 = read  "  U       U     U  \n\
                       \  0 1 2 3 4 5 6 7  \n\
                       \0                 0\n\
                       \1                 1\n\
                       \2                 2\n\
                       \3                 3\n\
                       \4               ♚ 4\n\
                       \5                 5\n\
                       \6 ♟               6\n\
                       \7               ♔ 7\n\
                       \  0 1 2 3 4 5 6 7  \n\
                       \  U       U     U\n\
                       \[Black]"
        move1 = NormalMove (Pos 4 7) (Pos 5 6)
        board2 = applyMove move1 board1
        board2' = read  "  U       U     U  \n\
                        \  0 1 2 3 4 5 6 7  \n\
                        \0                 0\n\
                        \1                 1\n\
                        \2                 2\n\
                        \3                 3\n\
                        \4                 4\n\
                        \5             ♚   5\n\
                        \6 ♟               6\n\
                        \7               ♔ 7\n\
                        \  0 1 2 3 4 5 6 7  \n\
                        \  U       U     U\n\
                        \[White]"
        moves = movesFun board2
        onlyMove = NormalMove (Pos 7 7) (Pos 7 6)

        board3 = applyMove onlyMove board2
        board3' = read  "  U       U     U  \n\
                        \  0 1 2 3 4 5 6 7  \n\
                        \0                 0\n\
                        \1                 1\n\
                        \2                 2\n\
                        \3                 3\n\
                        \4                 4\n\
                        \5             ♚   5\n\
                        \6 ♟               6\n\
                        \7             ♔   7\n\
                        \  0 1 2 3 4 5 6 7  \n\
                        \  U       U     U\n\
                        \[Black]"
        moves3 = [Promote (Pos 6 0) (Pos 7 0) Rook,
                  Promote (Pos 6 0) (Pos 7 0) Queen]


posDist :: Pos -> Pos -> Int
posDist (Pos r1 c1) (Pos r2 c2) = max (abs (r1 - r2)) (abs (c1 - c2))

--------------------------------------------------------------------------------
-- Whitebox testing
--------------------------------------------------------------------------------

prop_numberOfMovesTieBreak :: Property
prop_numberOfMovesTieBreak = verifyMakesMove' expMove board 3
    where
        board = read  "  U       U     U  \n\
                      \  0 1 2 3 4 5 6 7  \n\
                      \0 ♜ ♖   ♝ ♚     ♜ 0\n\
                      \1       ♘     ♛   1\n\
                      \2 ♛   ♗     ♕   ♘ 2\n\
                      \3             ♟   3\n\
                      \4           ♛ ♘ ♕ 4\n\
                      \5   ♞ ♛     ♟ ♝   5\n\
                      \6     ♘     ♝ ♜ ♖ 6\n\
                      \7       ♞ ♔     ♖ 7\n\
                      \  0 1 2 3 4 5 6 7  \n\
                      \  U       U     U\n\
                      \[Black]"
        expMove = NormalMove (Pos 1 6) (Pos 1 3)

        -- TODO: This shows a bug - doesn't do check?




                                  
    
-- TODO: Test move that needs two steps ahead thinking

-- TODO: Use a real game as inspiration. For tricky situations, test that the
-- computer does the correct thing


--------------------------------------------------------------------------------
-- Arbitrary
--------------------------------------------------------------------------------

prop_legalMove :: Board -> Property
prop_legalMove board = not (null legalMoves) ==>
                       -- TODO: Test for more depths when the algorithm is faster
                       conjoin [verifyMakesOneOfMoves' legalMoves board d | d <- [1..2]]
    where
        legalMoves = movesFun board

-- TODO: escape from check

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

-- TODO: make depth a parameter to more functions
depths :: [Int]
depths = [2..4]

nonKingKinds :: [Kind]
nonKingKinds = [Queen, Rook, Bishop, Knight, Pawn]

verifyMakesMove :: Move -> Board -> Property
verifyMakesMove expMove = verifyMakesOneOfMoves [expMove]

verifyMakesMove' :: Move -> Board -> Int -> Property
verifyMakesMove' expMove = verifyMakesOneOfMoves' [expMove]

verifyMakesOneOfMoves :: [Move] -> Board -> Property
verifyMakesOneOfMoves expMoves board =
    conjoin [verifyMakesOneOfMoves' expMoves board d | d <- depths]

verifyMakesOneOfMoves' :: [Move] -> Board -> Int -> Property
verifyMakesOneOfMoves' expMoves board depth =
    counterexample errorString result
    where
        result = move `elem` expMoves
        move = selectMove depth board 
        errorString = show board ++ "\n" ++
                      "Expected moves: " ++ show expMoves ++ "\n\n" ++
                      "Actual move: " ++ show move ++ "\n\n" ++
                      "Depth: " ++ show depth

verifyDoesNotCastle :: Board -> Bool
verifyDoesNotCastle board = all pred depths
    where
        -- I used to check "does not make move", but that is risky with castle since
        -- if color by mistake is flipped, it will return true for the wrong reason.
        pred depth = not $ isCastle $ selectMove depth board


return []
runTests = $quickCheckAll
