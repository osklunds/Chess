
# Chess

## Ideas

UI: Basic HTML and Javascript, and console

Generate an actual tree structure when doing alpha beta pruning, and save it. The opponent makes a move. Remove everything else from the tree, and just generate the lowest layer.

### Performance

https://stackoverflow.com/questions/9964496/alpha-beta-move-ordering

Move ordering:
- Sort by evalauted value and then continue recursively
- HashMap with cached values
- Store the entire calculated tree
- Also, clean up AB algo so that not always first etc. It should work now