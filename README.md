
# Chess

Chess AI written in Haskell.

# Ideas to work in

- Rules
    - Promoting
        - ~~Initial implementation~~
        - ~~Fix missunderstood behavior~~
    - Castling
        - ~~Stateless~~
        - ~~Remember if rooks/king have been moved~~
    - En passant
- Move generation
    - ~~Don't generate illegal moves from the start.
      https://peterellisjones.com/posts/generating-legal-chess-moves-efficiently/ ~~ Won't do because it changes the board representation too much, and part of the reason for this project is to use the current board representation for fun. 
- Move selection
    - ~~fix the issue about move order, i.e. that need to do what is short-term
      good as well, otherwise the good move is "procrastinated" forever.~~
- Score
    - Give more weight to squares in the center
    - Take into account the number of attacked squares
- Testing
    - More invariants for e.g. Board
    - More unit tests for move generation, move selection, and everything else
- Move selection tests
    - Checkmate with King+Rook
    - Checkmate with King+Queen
    - End game scenarios, with few pieces remaining. Check that the algorithm
    - Can checkmate within reasonable steps.
- Misc
    - ~~Moves are re-calculated so many times. For generating check aware moves,
      calculating game result, calculating the score, etc. This should be
      optimized~~
      - For making check aware: yes, that's the big cost. But after some math, only 2% is saved if the moves generated are re-used for the next depth. So it's not worth it.
      - Checking if can move at all: since only one move is needed, it's cheap anyway (due to laziness) when there are many moves. And if not many moves, probably few pieces, so cheap in that case too.
      - Checking if threatened: mainly relevant for UI, so that part is optimized. The other time is for checking if a draw if current player can't make a move. But such cases are not that common.
      - Conclusion: with the current board representation, can't optimize this more.
    - ~~Move Score to top-level~~
    - ~~Test promotes with MoveSelection~~
- Performance
    - Try to make other parts of move generation and score parallel. Note that I haven't been able to make Cabal run parallel at all yet. So I don't know if MiniMaxPar actually is faster than MiniMax.
    - Note: I tried Data.MemoTrie, but it consumed several gigabytes, so it wasn't practical. But see the memotrie branch. So efficient calculation might be needed after all if caching is infeasable. Unclear if AlphaBetaMemo is good.

# Dev

cabal repl --ghc-options="-fobject-code -O2"
