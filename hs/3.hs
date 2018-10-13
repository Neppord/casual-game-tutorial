#! /usr/bin/env stack
-- stack --resolver lts-12.9 script

import Graphics.Gloss

width = 640
blockWidth = 100
halfWidth = width `div` 2

window = InWindow "Utmaning 3" (width, 480) (10, 10)

render sec =
  translate xPos 0 block
  where
    xPos = fromIntegral $
      (round scaledTime `rem` width) - halfWidth
    scaledTime = sec * 100
    block =
      color blue $
      rectangleSolid blockWidth 250

main = animate window white render
