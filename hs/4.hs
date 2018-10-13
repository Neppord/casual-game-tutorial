#! /usr/bin/env stack
-- stack --resolver lts-12.9 script 
{-# LANGUAGE NamedFieldPuns #-}

import System.IO
import System.Random
  ( newStdGen
  , randomR
  )

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Point.Arithmetic ((+))

import Four.Data (Drop(..), State(..))
import Four.Render (render)

-- config
fps = 2
windowWidth :: Num a => a
windowWidth = 640 
windowHeight :: Num a => a
windowHeight = 480
windowPosition = (10, 10)
windowDimensions = (windowWidth, windowHeight)
background = blue

-- program
window = InWindow "rain" windowDimensions windowPosition
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
  g <- newStdGen
  let halfWidth = windowWidth / 2 :: Float
  let halfHeight = windowHeight / 2 :: Float
  let (x, _) = randomR (-halfWidth, halfWidth) g 
  let y = halfHeight
  -- mainFunction $ State {bg, rainDrop=Drop {pos=(x, y), mass=5}}
  return ()
