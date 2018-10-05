#! /usr/bin/env stack
-- stack --resolver lts-12.9 script 
{-# LANGUAGE NamedFieldPuns #-}

import Prelude hiding ((+))
import System.IO

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Point.Arithmetic ((+))

-- config
fps = 2
windowWidth = 640
windowHeight = 480
windowPosition = (10, 10)
windowDimensions = (windowWidth, windowHeight)
background = blue

-- Data

data Drop = Drop
  { pos :: Point
  , mass :: Float
  }

data State = State
  { bg :: Picture
  , rainDrop :: Drop
  }

-- program
window = InWindow "rain" windowDimensions windowPosition

renderRaindrop :: Drop -> Picture
renderRaindrop Drop {pos=(x, y), mass} =
  color blue
  $ translate x y
  $ rectangleSolid 2 mass

render State {bg, rainDrop} = Pictures [bg, renderRaindrop rainDrop]

handle events = id

physics time = id

mainFunction initState = play
  window
  background
  fps
  initState
  render
  handle
  physics

main = do
  print "hello world!"
  bg <- loadBMP "bg.bmp"
  -- mainFunction $ State {bg, rainDrop=Drop {pos=(50, 50), mass=5}}
  return ()
