module Rendering where

import Data.Array.IArray
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as P
import Graphics.Gloss.Interface.Pure.Game

import Configuration
import Logic
import Types

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

-- TODO: tidy
-------------

data State =
  State
    { grid :: Grid
    , paused :: Bool
    , time :: Float
    }

-- TODO: different color rendering for when it's paused?
-- TODO: reading cell size from command line isn't respected in rendering
render :: State -> Picture
render State { grid = g } = drawGrid g

placeCell :: Point -> State -> State
placeCell (x, y) game@(State { grid = g }) = game { grid = g' }
  where
    xOff = (fromIntegral (fst windowSize) - cellSize) / 2
    yOff = (fromIntegral (snd windowSize) - cellSize) / 2
    x' = round $ (x + xOff) / cellSize
    y' = round $ (y + yOff) / cellSize
    pos = (x', y')
    newValue = case g ! pos of
      Alive -> Dead
      Dead -> Alive
    g' = g // [(pos, newValue)]

-- TODO: mouse clicks
handleEvent :: Event -> State -> State
handleEvent (EventKey (Char 'p') Down _ _) game@(State _ p _) =
  game {paused = not p}
handleEvent
    (EventKey (MouseButton LeftButton) Down _ (x, y))
    game@(State g True _) = placeCell (x, y) game
handleEvent _ game = game

-- How often the game updates
updateSpeed :: Float
updateSpeed = 0.1

updateState :: Float -> State -> State
updateState elapsed game@(State g p t)
  | p = game
  | t < updateSpeed = game { time =  t + elapsed}
  | otherwise = game { grid = tick g, time = 0 }

-- playGOL :: (Int, Int) -> IO ()
-- playGOL size =
playGOL :: IO ()
playGOL =
  play display background fps initialState render handleEvent updateState
  where
    display = InWindow "Game of Life" windowSize (200, 200)
    fps = 60
    initialState = State (blankGrid gridSize) True 0
