module Four.PhysicsSpec (spec) where

import Prelude hiding ((-))

import Graphics.Gloss.Data.Point.Arithmetic ((-))
import Test.Hspec

import Four.Data

fall :: Float -> Drop -> Drop
fall height drop = drop {pos=newPos}
  where
    newPos = pos drop - (height, 0)

spec = 
  describe "fall" $ do
    let zerroDrop = Drop {pos=(0,0), mass=0}
    let oldDrop = Drop {pos=(-4,0), mass=0}
    it "decrement the height of a drop by a fixed amount" $ do
      fall 3 zerroDrop `shouldBe`
        Drop {pos=(-3, 0), mass=0}
      fall 2 zerroDrop `shouldBe`
        Drop {pos=(-2, 0), mass=0}
      fall 2 oldDrop `shouldBe`
        Drop {pos=(-6, 0), mass=0}
