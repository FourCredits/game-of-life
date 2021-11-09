module UserInteraction
  ( handleEvent
  ) where

import Graphics.Gloss.Interface.Pure.Game

import Types
import Logic
import Conversion

handleEvent :: Event -> State -> State
handleEvent (EventKey key Down _ (x, y)) game@State {paused = p} =
  case key of
    (Char 'p')                   -> game {paused = not p}
    (Char 'r')                   -> reset game
    (MouseButton LeftButton) | p -> clickOnCell (x, y) game
    _                            -> game
handleEvent _ game = game

clickOnCell :: Point -> State -> State
clickOnCell point game@State {grid = g} = game {grid = flipCell pos g}
  where
    pos = screenToGame point
