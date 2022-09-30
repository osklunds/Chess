
# Chess

Chess AI written in Haskell.

## Ideas to work in

- CLI
    - ~~Undo~~
    - ~~Read board from file~~
    - ~~Play as black too~~
    - Better UI
- Engine components
    - Optimizer
        - ~~Memoization in Alpha Beta~~
        - Move ordering
    - Move generation
        - Don't generate illegal moves from the start. https://peterellisjones.com/posts/generating-legal-chess-moves-efficiently/
    - Evaluator
        - Take position into account when calculating the score of a board
- Tests
    - Checkmate with King+Rook
    - Checkmate with King+Queen
- Rules
    - Promoting
    - Castling
    - En passant
    - 3 draw
    - 50 draw
- HTML UI
