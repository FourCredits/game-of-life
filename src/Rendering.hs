module Rendering
  ( window
  , render
  ) where

import Data.Array.IArray
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as P

import Configuration
import Conversion
import Logic
import Types

window :: Display
window = InWindow "Game of Life" windowSize (200, 200)

render :: State -> Picture
render State {grid = g, paused = p} =
  color cellColor . pictures . map (uncurry drawCell) . assocs $ g
  where
    cellColor =
      if p
        then pausedColor
        else runningColor

drawCell :: Index -> Cell -> Picture
drawCell pos cell = translate xOff yOff $ rectF cellSize cellSize
  where
    rectF =
      case cell of
        Dead -> rectangleWire
        Alive -> rectangleSolid
    (xOff, yOff) = gameToScreen pos
