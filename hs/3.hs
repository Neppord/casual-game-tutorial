#! /usr/bin/env stack
-- stack --resolver lts-12.9 script

import Graphics.Gloss

window = InWindow "Utmaning 3" (640, 480) (10, 10)

render = const Blank

main = animate window white render
