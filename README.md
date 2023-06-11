
# Chess

Chess AI written in Haskell.

## Short term plan for order of things to work on

### Must haves

- Clean up CheckAware
- Move Score to top-level
- Make all tests pass and all code compile
- Test promotes with MoveSelection

- Stateful castling

- En passant

- Fix the super-hard M1,M2 vs M2,M1 move issue

- More MoveSelection tests

- Take position on board into account when calculating score

- Faster move generation

### Nice to haves

- More tests for board
- More asserts everywhere
- More tests for Optimize

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
- Move selection tests
    - Checkmate with King+Rook
    - Checkmate with King+Queen
    - End game scenarios, with few pieces remaining. Check that the algorithm
    - Can checkmate within reasonable steps.
