#! /usr/bin/env stack
-- stack --resolver lts-12.9 script 
import Graphics.Gloss

windowWidth = 640
windowHeight = 480
windowDimensions = (windowWidth, windowHeight)
window = InWindow "Hello World" windowDimensions (10, 10)

background = white
fps = 30

render (x, y)
  = Translate x y
  $ Scale 0.5 0.5
  $ Text "Hello World"

handle = const id
physics = const id

main = play
  window 
  background
  fps
  (-170, -20)
  render
  handle
  physics


