module Logic where

import Data.Array.IArray
import Types

-- Constructs a blank grid with the given width and height
blankGrid :: Int -> Int -> Grid
blankGrid w h = listArray ((0, 0), (w - 1, h - 1)) (repeat Dead)

-- Constructs a grid where the given cells are all alive, and the rest are dead
makeGrid :: Int -> Int -> [Index] -> Grid
makeGrid w h indexes = blankGrid w h // (zip indexes $ repeat Alive)

-- Count how many alive neighbours the given position has
countNeighbours :: Grid -> Index -> Int
countNeighbours grid pos@(r, c) = length $ filter pred neighbours
  where
    neighbours = range ((r - 1, c - 1), (r + 1, c + 1))
    pred pos' = (pos /= pos') && inRange b pos' && grid ! pos' == Alive
    b = bounds grid

-- The rules of the game of life, the very essence of it all:
-- - If a alive cell has 2 or 3 alive neighbours, it survives.
-- - If a alive cell has any other amount of alive neighbours, it dies.
-- - If a dead cell has exactly 3 alive neighbours, it comes alive.
updateCell :: Cell -> Int -> Cell
updateCell Alive 2 = Alive
updateCell _ 3 = Alive
updateCell _ _ = Dead

-- Goes through the grid, updating each cell in turn
tick :: Grid -> Grid
tick grid = array bnds $ map f $ range bnds
  where
    bnds = bounds grid
    f pos = (pos, updateCell (grid ! pos) (countNeighbours grid pos))
