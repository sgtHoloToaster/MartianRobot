module LibSpec (spec) where

import Test.Hspec
import Lib(parseCommand, Command(..))

spec :: Spec
spec = do
  describe "parseCommand" $ do
    it "doesn't parse invalid command" $ do
      parseCommand "I'am an invalid one" `shouldBe` Nothing