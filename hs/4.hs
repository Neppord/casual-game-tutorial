#! /usr/bin/env stack
-- stack --resolver lts-12.9 script 

import System.IO

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


-- config
fps = 2
windowWidth = 640
windowHeight = 480
windowPosition = (10, 10)
windowDimensions = (windowWidth, windowHeight)
background = white

-- program
window = InWindow "rain" windowDimensions windowPosition

render = id

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
  bg <- loadBMP "bg.bmp"
  mainFunction bg
