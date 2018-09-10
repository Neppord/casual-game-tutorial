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

render (True, s, d) = do
  return
  $ Pictures [aText, bText, cText]
  where
    aText = Translate (-200) 0 $ Text "A"
    bText = Translate 0 0 $ Text "S"
    cText = Translate 200 0 $ Text "D"
render (False, s, d) = do
  return
  $ Pictures [bText, cText]
  where
    bText = Translate 0 0 $ Text "S"
    cText = Translate 200 0 $ Text "D"

handle (EventKey (Char 'a') Down _ _) (_, b, c) = do
  return (True, b, c)
handle (EventKey (Char 'a') Up _ _) (_, b, c) = do
  return (False, b, c)
handle event world = do
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
