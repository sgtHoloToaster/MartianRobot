module Tests (spec) where

import Test.Hspec
import MartianRobot

spec :: Spec
spec = do
  describe "work" $ do
    it "works" $ do
      "value" `shouldBe` "value"