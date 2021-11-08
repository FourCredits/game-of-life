module Rendering where

import Data.Array.IArray
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as P

import Types
import Configuration

drawGrid :: Grid -> Picture
drawGrid =
  translate x y . pictures . map (uncurry drawCell) . assocs
  where
    x = (cellSize - fromIntegral (fst windowSize)) / 2
    y = (cellSize - fromIntegral (snd windowSize)) / 2

drawCell :: Index -> Cell -> Picture
drawCell pos cell =
  translate xOff yOff $ color cellColor $ rectF cellSize cellSize
  where
    rectF =
      case cell of
        Dead -> rectangleWire
        Alive -> rectangleSolid
    (xOff, yOff) = cellSize P.* toPoint pos

cellSize :: Float
cellSize = uncurry min $ cast2 (/) (toPoint windowSize) (toPoint gridSize)

cast :: (a -> b) -> (a, a) -> (b, b)
cast f (x1, x2) = (f x1, f x2)

cast2 :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
cast2 f (x1, x2) (y1, y2) = (f x1 y1, f x2 y2)

toPoint :: (Int, Int) -> Point
toPoint = cast fromIntegral
