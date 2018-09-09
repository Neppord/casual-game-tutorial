#! /usr/bin/env stack
-- stack --resolver lts-12.9 script 
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

windowWidth = 640
windowHeight = 480
windowDimensions = (windowWidth, windowHeight)
window = InWindow "Hello World" windowDimensions (10, 10)

background = white
fps = 30

render :: (Integer, Integer) -> IO Picture
render (x, y) = do
  return
  $ Translate (fromIntegral x) (fromIntegral y)
  $ Scale 0.5 0.5
  $ Text "Hello World"

handle events world = do
  return world
physics time world = do
  return world

initState = (-170, -20)
main = playIO
  window 
  background
  fps
  initState
  render
  handle
  physics


