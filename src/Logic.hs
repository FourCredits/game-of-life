module Logic
  ( reset
  , updateState
  , initialState
  , flipCell
  ) where

import Data.Array.IArray

import Types
import Configuration

-- Update the state of the game, only if enough time has passed
updateState :: Float -> State -> State
updateState elapsed game@(State g p t)
  | p = game
  | t < updatePeriod = game { time =  t + elapsed}
  | otherwise = game { grid = tick g, time = 0 }

-- Create the initial state of the game
initState :: (Int, Int) -> State
initState size = State {grid = blankGrid size, paused = True, time = 0}

initialState :: State
initialState = initState gridSize

-- Reset a game back to its initial state
reset :: State -> State
reset _ = initialState

-- Constructs a blank grid with the given width and height
blankGrid :: (Int, Int) -> Grid
blankGrid (w, h) = listArray ((0, 0), (w - 1, h - 1)) (repeat Dead)

-- Changes a given index from Dead to Alive, or Alive to Dead
flipCell :: Index -> Grid -> Grid
flipCell pos grid = grid // [(pos, val')]
  where
    val' =
      case grid ! pos of
        Alive -> Dead
        Dead -> Alive

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
