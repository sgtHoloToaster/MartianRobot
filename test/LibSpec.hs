module LibSpec (spec) where

import Test.Hspec
import Lib(
    parseCommand
  , parseCommands
  , Command(..)
  , Direction(..)
  , turnRight
  , turnLeft
  , Position(..)
  , moveForward
  , executeCommands
  , executeCommand)

spec :: Spec
spec = do
  describe "parseCommand" $ do
    it "doesn't parse invalid command" $ do
      parseCommand 'y' `shouldBe` Nothing

    it "parses a command" $ do
      parseCommand 'L' `shouldBe` Just TurnLeft
      parseCommand 'R' `shouldBe` Just TurnRight
      parseCommand 'F' `shouldBe` Just MoveForward

    it "parses a set of valid commands" $ do
      parseCommands "LFRFF" `shouldBe` [Just TurnLeft, Just MoveForward, Just TurnRight, Just MoveForward, Just MoveForward]

    it "doesn't fall when encoutered an invalid command in a set of commands" $ do
      parseCommands "LYkRFR" `shouldBe` [Just TurnLeft, Nothing, Nothing, Just TurnRight, Just MoveForward, Just TurnRight]

    let initPosition direction = Position { coordinates = (1, 1), direction = direction } in do
      it "turns right" $ do
        let p = initPosition North in turnRight p `shouldBe` p { direction = East }
        let p = initPosition East in turnRight p `shouldBe` p { direction = South }
        let p = initPosition South in turnRight p `shouldBe` p { direction = West }
        let p = initPosition West in turnRight p `shouldBe` p { direction = North }

      it "turns left" $ do
        let p = initPosition North in turnLeft p `shouldBe` p { direction = West }
        let p = initPosition East in turnLeft p `shouldBe` p { direction = North }
        let p = initPosition South in turnLeft p `shouldBe` p { direction = East }
        let p = initPosition West in turnLeft p `shouldBe` p { direction = South }

      it "changes coordinates by moving forward" $ do
        let p = initPosition North in moveForward p `shouldBe` p { coordinates = (1, 2) }
        let p = initPosition East in moveForward p `shouldBe` p { coordinates = (2, 1) }
        let p = initPosition South in moveForward p `shouldBe` p { coordinates = (1, 0) }
        let p = initPosition West in moveForward p `shouldBe` p { coordinates = (0, 1) }

      it "executes a TurnLeft command" $ do
        let p = initPosition North in executeCommand p TurnLeft `shouldBe` p { direction = West }

      it "executes a TurnRight command" $ do
        let p = initPosition North in executeCommand p TurnRight `shouldBe` p { direction = East }

      it "executes a MoveForward command" $ do
        let p = initPosition North in executeCommand p MoveForward `shouldBe` p { coordinates = (1, 2) }

      it "executes a set of commands" $ do
        let p = initPosition North in 
          executeCommands p [MoveForward, TurnLeft, MoveForward, TurnRight] `shouldBe` p { coordinates = (0, 2) }
