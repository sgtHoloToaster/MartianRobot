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
  , executeCommand
  , AreaSize(..)
  , CommandExecutionResult(..)
  , parseAndExecuteCommand
  , parseAndExecuteCommands)

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

    let areaSize = (0, 1) in do
      it "changes coordinates when the correct command is provided" $ do
        let initPosition = Position { coordinates = (0, 0), direction = North } in
          parseAndExecuteCommand areaSize initPosition 'F' `shouldBe` CommandExecutionResult {
            position = initPosition { coordinates = (0, 1) },
            outOfArea = False
          }

      it "changes direction when the correct command is provided" $ do
        let initPosition = Position { coordinates = (0, 0), direction = North } in
          parseAndExecuteCommand areaSize initPosition 'L' `shouldBe` CommandExecutionResult {
            position = initPosition { direction = West },
            outOfArea = False
          }

      it "is out of the area when moves over the bottom border" $ do
        let initPosition = Position { coordinates = (0, 0), direction = South } in
          parseAndExecuteCommand areaSize initPosition 'F' `shouldBe` CommandExecutionResult {
            position = initPosition { coordinates = (0, -1) },
            outOfArea = True
          }

      it "is out of the area when moves over the right border" $ do
        let initPosition = Position { coordinates = (0, 0), direction = East } in
          parseAndExecuteCommand areaSize initPosition 'F' `shouldBe` CommandExecutionResult {
            position = initPosition { coordinates = (1, 0) },
            outOfArea = True
          }
        
    let areaSize = (3,2) 
    let initPosition = Position { coordinates = (0, 0), direction = North } in do
      it "returns initial position when no commands are provided" $ do
        parseAndExecuteCommands areaSize initPosition "" `shouldBe` CommandExecutionResult {
          position = initPosition,
          outOfArea = False
        }

      it "returns initial position when all provided commands are incorrect" $ do
        parseAndExecuteCommands areaSize initPosition "TPMMN" `shouldBe` CommandExecutionResult {
          position = initPosition,
          outOfArea = False
        }

      it "returns the last coordinates where the robot fell out the area" $ do
        parseAndExecuteCommands areaSize initPosition "FRFLFFFFFFLFLFFF" `shouldBe` CommandExecutionResult {
          position = initPosition{ coordinates = (1, 3) },
          outOfArea = True
        }

      it "returns the last coordinates where the robot stopped" $ do
        parseAndExecuteCommands areaSize initPosition "FRFRFRF" `shouldBe` CommandExecutionResult {
          position = Position{ coordinates = (0, 0), direction = West },
          outOfArea = False
        }
