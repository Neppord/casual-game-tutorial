#! /usr/bin/env stack
-- stack --resolver lts-12.9 script 

import System.IO

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


-- config
fps = 30
background = white
windowWidth = 640
windowHeight = 480
windowDimensions = (windowWidth, windowHeight)
window = InWindow "Trummaskin" windowDimensions (10, 10)

-- program
printDebug value = do
  print value
  hFlush stdout

render (a, s, d) = do
  return
  $ Pictures [aText, bText, cText]
  where
    aText = Translate (-200) 0 $ Text "A"
    bText = Translate 0 0 $ Text "S"
    cText = Translate 200 0 $ Text "D"

handle events world = do
  return world

physics time world = do
  return world

mainFunction initState = playIO
  window
  background
  fps
  initState
  render
  handle
  physics

main = do
  mainFunction (False, False, False)
