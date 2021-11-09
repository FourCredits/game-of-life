module Configuration where

import Graphics.Gloss.Data.Color

background :: Color
background = black

runningColor :: Color
runningColor = green

pausedColor :: Color
pausedColor = greyN 0.5

-- The width and height of the grid in the grid
gridSize :: (Int, Int)
gridSize = (40, 40)

-- How big the window is, in pixels
windowSize :: (Int, Int)
windowSize = (400, 400)

-- The update period, in seconds
updatePeriod :: Float
updatePeriod = 0.1

-- How fast the game updates. Doesn't really have to be that high
fps :: Int
fps = 24
