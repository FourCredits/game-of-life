module Main where

import Graphics.Gloss
import System.Environment

import Logic
import Configuration
import Rendering

main :: IO ()
-- main =
--   display
--     (InWindow "Game of Life" windowSize (200, 200))
--     background
--     (drawGrid $ blankGrid gridSize)
main = do
  -- TODO: better error handling
  -- args <- getArgs
  -- let [w, h] = map read args
  -- playGOL (w, h)
  playGOL
