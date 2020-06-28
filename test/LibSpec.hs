module LibSpec (spec) where

import Test.Hspec
import Lib(parseCommand, parseCommands, Command(..), Direction(..), turnRight, turnLeft, Position(..), moveForward)

spec :: Spec
spec = do
  describe "parseCommand" $ do
    it "doesn't parse invalid command" $ do
      parseCommand 'y' `shouldBe` Nothing

    it "parses a command" $ do
      parseCommand 'L' `shouldBe` Just TurnLeft
      parseCommand 'R' `shouldBe` Just TurnRight
      parseCommand 'F' `shouldBe` Just MoveForward

    it "parses list of valid commands" $ do
      parseCommands "LFRFF" `shouldBe` [Just TurnLeft, Just MoveForward, Just TurnRight, Just MoveForward, Just MoveForward]

    it "doesn't fall when encoutered an invalid command in a command list" $ do
      parseCommands "LYkRFR" `shouldBe` [Just TurnLeft, Nothing, Nothing, Just TurnRight, Just MoveForward, Just TurnRight]

    it "turns right" $ do
      turnRight North `shouldBe` East
      turnRight East `shouldBe` South
      turnRight South `shouldBe` West
      turnRight West `shouldBe` North

    it "turns left" $ do
      turnLeft North `shouldBe` West
      turnLeft West `shouldBe` South
      turnLeft South `shouldBe` East
      turnLeft East `shouldBe` North

    it "changes coordinates by moving forward" $ do
      let initPosition direction = Position { coordinates = (1, 1), direction = direction } in do
        let p = initPosition North in moveForward p `shouldBe` p { coordinates = (1, 2) }
        let p = initPosition East in moveForward p `shouldBe` p { coordinates = (2, 1) }
        let p = initPosition South in moveForward p `shouldBe` p { coordinates = (1, 0) }
        let p = initPosition West in moveForward p `shouldBe` p { coordinates = (0, 1) }