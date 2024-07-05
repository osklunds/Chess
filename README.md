
# Chess

Chess AI written in Haskell.

# Ideas to work in

- Rules
    - Promoting
        - ~~Initial implementation~~
        - ~~Fix missunderstood behavior~~
    - Castling
        - ~~Stateless~~
        - Remember if rooks/king have been moved
    - En passant
- Move generation
    - Don't generate illegal moves from the start.
      https://peterellisjones.com/posts/generating-legal-chess-moves-efficiently/
- Move selection
    - fix the issue about move order, i.e. that need to do what is short-term
      good as well, otherwise the good move is "procrastinated" forever.
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
    - Moves are re-calculated so many times. For generating check aware moves,
      calculating game result, calculating the score, etc. This should be
      optimized
    - ~~Move Score to top-level~~
    - Test promotes with MoveSelection

# Dev

cabal repl --ghc-options="-fobject-code -O2"
