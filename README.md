
# Chess

Chess AI written in Haskell.

## Ideas to work in

- Rules
    - Promoting
        - ~~Initial implementation~~
        - Fix missunderstood behavior
    - Castling
        - ~~Stateless~~
        - Remember if rooks/king have been moved
    - En passant
- Move generation
    - Don't generate illegal moves from the start.
      https://peterellisjones.com/posts/generating-legal-chess-moves-efficiently/
- Legacy improvements
    - Skip separate "normal moves" module
    - Unit tests with hard coded boards. Filter actual and expected moves based
      on kind.
    - Add more invariants to e.g. Board, but remove unit tests of invariants
    - When the above legacy improvements are done, fix the issue about move
      order, i.e. that need to do what is short-term good as well, otherwise the
      good move is "procrastinated" forever.
- Tests
    - Checkmate with King+Rook
    - Checkmate with King+Queen
