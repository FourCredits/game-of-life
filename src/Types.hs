module Types where

import Data.Array.IArray

data Cell
  = Alive
  | Dead
  deriving (Show, Eq)

type Index = (Int, Int)

type Grid = Array Index Cell

data State =
  State
    { grid :: Grid
    , paused :: Bool
    , time :: Float
    }

