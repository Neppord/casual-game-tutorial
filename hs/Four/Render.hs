{-# LANGUAGE NamedFieldPuns #-}
module Four.Render (render) where

import Graphics.Gloss
  ( translate
  , color
  , blue
  , rectangleSolid
  , Picture
  )

import Four.Data (Drop(..), State(..))

renderRaindrop :: Drop -> Picture
renderRaindrop Drop {pos=(x, y), mass} =
  color blue
  $ translate x y
  $ rectangleSolid 2 mass

render State {bg, rainDrop} =
  bg <>
  renderRaindrop rainDrop

