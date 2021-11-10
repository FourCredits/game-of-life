module Conversion
  ( cellSize
  , gameToScreen
  , screenToGame
  ) where

import Data.Function
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as P

import Configuration
import Types

-- Module for converting between game coordinates and screen coordinates

screenToGame :: Point -> Index
screenToGame point = roundPair $ (1 / cellSize) P.* (point P.+ offset)

gameToScreen :: Index -> Point
gameToScreen pos = (cellSize P.* fromIntegralPair pos) P.- offset

cellSize :: Float
cellSize = uncurry min $ (divPair `on` fromIntegralPair) windowSize gridSize

offset :: (Float, Float)
offset = 0.5 P.* (fromIntegralPair windowSize P.- (cellSize, cellSize))

-- Utils for working with pairs of numbers

roundPair :: (Float, Float) -> (Int, Int)
roundPair (x, y) = (round x, round y)

fromIntegralPair :: (Int, Int) -> (Float, Float)
fromIntegralPair (x, y) = (fromIntegral x, fromIntegral y)

divPair :: (Float, Float) -> (Float, Float) -> (Float, Float)
divPair (a, b) (c, d) = (a / c, b / d)
