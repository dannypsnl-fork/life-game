module LifeGame.NextSpec where

import LifeGame.Cell
import LifeGame.Field
import LifeGame.Next
import Test.Hspec

spec :: Spec
spec = describe "life game" $ do
  context "next cell" $ do
    it "around with 3 alive will alive" $ do
      let f =
            Field
              [ [Alive, Empty, Empty],
                [Alive, Empty, Empty],
                [Alive, Empty, Empty]
              ]
      nextCell f (0, 0) `shouldBe` Alive
