#! /usr/bin/env stack
-- stack --resolver lts-12.9 script 
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- config
fps = 30
background = white
windowWidth = 640
windowHeight = 480
initState = (-170, -20)
windowDimensions = (windowWidth, windowHeight)
window = InWindow "Hello World" windowDimensions (10, 10)

-- functions
render (x, y) = do
  return
  $ Translate (fromIntegral x) (fromIntegral y)
  $ Scale 0.5 0.5
  $ Text "Hello World"

handle events world = do
  return world

physics time world = do
  return world

main = playIO
  window 
  background
  fps
  initState
  render
  handle
  physics


