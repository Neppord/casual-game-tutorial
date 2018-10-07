#! /usr/bin/env stack
-- stack --resolver lts-12.9 script 

import System.IO

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


-- config
fps = 2
displayText = "Hello World"
textScale = 0.5
stateFile = "1.txt"
windowWidth = 640
windowHeight = 480
windowPosition = (10, 10)
windowDimensions = (windowWidth, windowHeight)
background = white


-- program
window = InWindow displayText windowDimensions windowPosition

printDebug value = do
  print value
  hFlush stdout

readState = do
  fileContent <- readFile stateFile
  let [x, y] = words fileContent
  return (read x, read y)

render (x, y) = return
  $ Translate (fromIntegral x) (fromIntegral y)
  $ Scale textScale textScale
  $ Text displayText

handle events = return

physics time world = readState

mainFunction initState = playIO
  window
  background
  fps
  initState
  render
  handle
  physics

main = do
  state <- readState
  mainFunction state
