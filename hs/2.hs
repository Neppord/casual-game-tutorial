#! /usr/bin/env stack
-- stack --resolver lts-12.9 runghc --package proteaaudio --package gloss
{-# LANGUAGE NamedFieldPuns #-}
import Control.Monad
import System.IO

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Sound.ProteaAudio


-- config
fps = 30
background = white
letterDistance = 150
windowWidth = 640
windowHeight = 480
windowPosition = (10, 10)
windowDimensions = (windowWidth, windowHeight)
window = InWindow "Trummaskin" windowDimensions windowPosition


-- data
data World = World
  { a :: Bool
  , s :: Bool
  , d :: Bool
  }


-- program
printDebug value = do
  print value
  hFlush stdout

pick bools values = [v | (b, v) <- zip bools values, b]
render World{a, s, d} =
 return $ Pictures $ pick [a, s, d] [aText, bText, cText]
  where
    aText = Translate (-letterDistance) 0 $ Text "A"
    bText = Translate 0 0 $ Text "S"
    cText = Translate letterDistance 0 $ Text "D"

playFile filename = do
  sample <- sampleFromFile filename 1.0
  soundPlay sample 1 1 0 1

playSoundA = playFile "a.wav"
playSoundS = playFile "s.wav"
playSoundD = playFile "d.wav"

handle (EventKey (Char 'a') Down _ _) world = do
  playSoundA
  return world {a = True}
handle (EventKey (Char 'a') Up _ _) world =
  return world {a = False}
handle (EventKey (Char 's') Down _ _) world = do
  playSoundS
  return world {s = True}
handle (EventKey (Char 's') Up _ _) world =
  return world {s = False}
handle (EventKey (Char 'd') Down _ _) world = do
  playSoundD
  return world {d = True}
handle (EventKey (Char 'd') Up _ _) world =
  return world {d = False}
handle _ = return


physics time = return

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
  mainFunction $ World { a = False, s = False, d = False}
  print "Shutting down audio"
  hFlush stdout
  finishAudio
