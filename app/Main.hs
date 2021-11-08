module Main where

import Graphics.Gloss

import Logic
import Configuration
import Rendering

main :: IO ()
main =
  display
    (InWindow "Game of Life" windowSize (200, 200))
    background
    (drawGrid $ blankGrid gridSize)
