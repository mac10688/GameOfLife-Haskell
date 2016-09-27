# GameOfLife-Haskell
Conway's Game of life implemented in Haskell

## Rules
The universe of the Game of Life is an infinite two-dimensional orthogonal grid of square cells, each of which is in one of two possible states, alive or dead, or "populated" or "unpopulated" (the difference may seem minor, except when viewing it as an early model of human/urban behavior simulation or how one views a blank space on a grid). Every cell interacts with its eight neighbours, which are the cells that are horizontally, vertically, or diagonally adjacent. At each step in time, the following transitions occur:

1. Any *__live__ cell with __fewer than two__ live neighbors __dies__*, as if caused by under-population.
2. Any *__live__ cell with __two or three__ live neighbors __lives__* on to the next generation.
3. Any *__live__ cell with __more than three__ live neighbors __dies__*, as if by over-population.
4. Any *__dead__ cell with __exactly three__ live neighbors becomes a __live__ cell*. as if by reproduction.

The initial pattern constitutes the seed of the system. The first generation is created by applying the above rules simultaneously to every cell in the seed—births and deaths occur simultaneously, and the discrete moment at which this happens is sometimes called a tick (in other words, each generation is a pure function of the preceding one). The rules continue to be applied repeatedly to create further generations.
