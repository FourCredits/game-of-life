module Main where

import Graphics.Gloss
import System.Environment

import Logic (initialState, updateState)
import Configuration (background, fps)
import Rendering (window, render)
import UserInteraction (handleEvent)

main :: IO ()
main = play window background fps initialState render handleEvent updateState
