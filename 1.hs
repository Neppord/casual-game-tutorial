#! /usr/bin/env stack
-- stack --resolver lts-12.9 script --ghc-options "-optl-Wl,-framework,GLUT"
import Graphics.Gloss

windowWidth = 640
windowHeight = 480
windowDimensions = (windowWidth, windowHeight)
window = InWindow "Hello World" windowDimensions (10, 10)
background = white
picture 
  = Translate (-170) (-20)
  $ Scale 0.5 0.5
  $ Text "Hello World"
main = display window background picture

