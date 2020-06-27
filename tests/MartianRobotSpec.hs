module MartianRobotSpec(spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "work" $ do
    it "works" $ do
      "value" `shouldBe` "value"