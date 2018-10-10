#! /usr/bin/env stack
-- stack --resolver lts-12.9 script 

import System.IO
import Control.Monad

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

scaledText =
  scale textScale textScale textPicture
  where textPicture = text displayText

at picture (x, y) = translate x y picture

render point = scaledText `at` point

handle events = return

physics time world = readState

mainFunction initState = playIO
  window
  background
  fps
  initState
  (return . render)
  handle
  physics

main = do
  state <- readState
  mainFunction state
