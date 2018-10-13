module Four.Data where

import Graphics.Gloss

data Drop = Drop
  { pos :: Point
  , mass :: Float
  }

data State = State
  { bg :: Picture
  , rainDrop :: Drop
  }

