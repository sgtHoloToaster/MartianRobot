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
      moveForward Position { coordinates = (1, 1), direction = North } `shouldBe` (1, 2)
      moveForward Position { coordinates = (1, 1), direction = East } `shouldBe` (2, 1)
      moveForward Position { coordinates = (1, 1), direction = South } `shouldBe` (1, 0)
      moveForward Position { coordinates = (1, 1), direction = West } `shouldBe` (0, 1)