#! /usr/bin/env stack
-- stack --resolver lts-12.9 script 

import System.IO

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


-- config
fps = 2
background = white
windowWidth = 640
windowHeight = 480
windowDimensions = (windowWidth, windowHeight)
window = InWindow "Hello World" windowDimensions (10, 10)
stateFile = "1.txt"

-- program
printDebug value = do
  print value
  hFlush stdout

readState = do
  fileContent <- readFile stateFile
  let [x, y] = words fileContent
  return (read x, read y)

render (x, y) = do
  return
  $ Translate (fromIntegral x) (fromIntegral y)
  $ Scale 0.5 0.5
  $ Text "Hello World"


handle events world = do
  return world

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
