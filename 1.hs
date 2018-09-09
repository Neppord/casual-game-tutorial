#! /usr/bin/env stack
-- stack --resolver lts-12.9 script 
import Graphics.Gloss

windowWidth = 640
windowHeight = 480
windowDimensions = (windowWidth, windowHeight)
window = InWindow "Hello World" windowDimensions (10, 10)

background = white
fps = 30

picture 
  = Translate (-170) (-20)
  $ Scale 0.5 0.5
  $ Text "Hello World"

render = const picture
handle = const id
physics = const id

main = play
  window 
  background
  fps
  ()
  render
  handle
  physics


