module Four.PhysicsSpec (spec) where

import Test.Hspec

import Four.Data

fall :: Float -> Drop -> Drop
fall height drop = drop {pos=(-3, 0)}

spec = 
  describe "fall" $ do
    it "decrement the height of a drop by a fixed amount" $ do
      let drop = Drop {pos=(0,0), mass=0}
      let result = fall 3 drop
      let expected = Drop {pos=(-3, 0), mass=0}
      result `shouldBe` expected
