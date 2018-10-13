module Four.Data where

import Graphics.Gloss

data Drop = Drop
  { pos :: Point
  , mass :: Float
  } deriving (Show, Eq)

data State = State
  { bg :: Picture
  , rainDrop :: Drop
  } deriving (Show, Eq)

