#! /usr/bin/env stack
-- stack --resolver lts-12.9 runghc --package proteaaudio --package gloss

import Control.Monad
import System.IO

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Sound.ProteaAudio

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


playSoundA = do
  sample <- sampleFromFile "a.wav" 1.0
  soundPlay sample 1 1 0 1


handle (EventKey (Char 'a') Down _ _) (_, b, c) = do
  playSoundA
  return (True, b, c)
handle (EventKey (Char 'a') Up _ _) (_, b, c) = do
  return (False, b, c)
handle _ world = do
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
  print "Initializing audio"
  result <- initAudio 64 44100 1024
  unless result $ fail "failed to initialize audio"
  mainFunction (False, False, False)
  print "Shutting down audio"
  hFlush stdout
  finishAudio
