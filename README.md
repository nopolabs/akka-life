akka-life
=========
Conway's game of life using akka and scala.

To run and test it use SBT invoke: 'sbt run'

;; The universe of the Game of Life is an infinite two-dimensional
;; orthogonal grid of square cells, each of which is in one of
;; two possible states, live or dead. Every cell interacts with
;; its eight neighbors, which are the cells that are directly
;; horizontally, vertically, or diagonally adjacent.
;;
;; At each step in time, the following transitions occur:
;;
;;  Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
;;  Any live cell with more than three live neighbours dies, as if by overcrowding.
;;  Any live cell with two or three live neighbours lives on to the next generation.
;;  Any dead cell with exactly three live neighbours becomes a live cell.

;; a cell is an location vector, e.g. [row col]
;;
;; a generation is a set of live cells
