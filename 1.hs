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

initState = (-170, -20)


-- program
printDebug value = do
  print value
  hFlush stdout

render (x, y) = do
  return
  $ Translate (fromIntegral x) (fromIntegral y)
  $ Scale 0.5 0.5
  $ Text "Hello World"

handle events world = do
  fileContent <- readFile "1.txt"
  let [x, y] = words fileContent
  return (read x, read y)

physics time world = do
  return world

main = playIO
  window 
  background
  fps
  initState
  render
  physics
  handle


