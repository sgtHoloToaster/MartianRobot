module Lib ( 
    parseCommand
  , parseCommands
  , Command(..)
  , Direction(..)
  , turnLeft
  , turnRight
  , moveForward
  , Position(..)
  , executeCommand
  , executeCommands
  , AreaSize(..)
  , CommandExecutionResult(..)
  , parseAndExecuteCommand
  , parseAndExecuteCommands
) where

data Command = 
    TurnLeft
  | TurnRight
  | MoveForward 
  deriving (Show, Eq)

parseCommand :: Char -> Maybe Command
parseCommand 'L' = Just TurnLeft
parseCommand 'R' = Just TurnRight
parseCommand 'F' = Just MoveForward
parseCommand _ = Nothing

parseCommands :: String -> [Maybe Command]
parseCommands commands = map parseCommand commands 

data Direction =
    North
  | East
  | South
  | West
  deriving (Show, Eq)

turnLeft :: Position -> Position
turnLeft p@Position { direction = North } = p { direction = West }
turnLeft p@Position { direction = West } = p { direction = South }
turnLeft p@Position { direction = South } = p { direction = East }
turnLeft p@Position { direction = East } = p { direction = North }

turnRight :: Position -> Position
turnRight p@Position { direction = North } = p { direction = East }
turnRight p@Position { direction = East } = p { direction = South }
turnRight p@Position { direction = South } = p { direction = West }
turnRight p@Position { direction = West } = p { direction = North }

type Coordinates = (Int, Int)
data Position = Position {
    coordinates :: Coordinates
  , direction :: Direction
} deriving (Eq, Show)

moveForward :: Position -> Position
moveForward p@Position { direction = North, coordinates = (x, y) } = p { coordinates = (x, y + 1) }
moveForward p@Position { direction = East, coordinates = (x, y) } = p { coordinates = (x + 1, y) }
moveForward p@Position { direction = South, coordinates = (x, y) } = p { coordinates = (x, y - 1) }
moveForward p@Position { direction = West, coordinates = (x, y) } = p { coordinates = (x - 1, y) }

executeCommand :: Position -> Command -> Position
executeCommand p TurnLeft = turnLeft p
executeCommand p TurnRight= turnRight p
executeCommand p MoveForward = moveForward p

executeCommands :: Position -> [Command] -> Position
executeCommands commands position = foldl executeCommand commands position

type AreaSize = (Int, Int)
data CommandExecutionResult = CommandExecutionResult {
    position :: Position
  , outOfArea :: Bool
} deriving (Eq, Show)

applyCommandParsingResult :: Position -> Maybe Command -> Position
applyCommandParsingResult position Nothing = position
applyCommandParsingResult position (Just command) = executeCommand position command

isOutOfArea :: AreaSize -> Position -> Bool
isOutOfArea _ p@Position{ coordinates = (-1, _) } = True
isOutOfArea _ p@Position{ coordinates = (_, -1) } = True
isOutOfArea (ax, ay) p@Position{ coordinates = (px, py) } = ax < px || ay < py

parseAndExecuteCommand :: AreaSize -> Position -> Char -> CommandExecutionResult
parseAndExecuteCommand areaSize position rawCommand = 
  let parsedCommand = parseCommand rawCommand in
    let newPosition = applyCommandParsingResult position parsedCommand in
      CommandExecutionResult { 
        position = newPosition,
        outOfArea = isOutOfArea areaSize newPosition
      }
      
parseAndExecuteCommands :: AreaSize -> Position -> String -> CommandExecutionResult
parseAndExecuteCommands areaSize currentPosition "" = CommandExecutionResult { position = currentPosition, outOfArea = isOutOfArea areaSize currentPosition }
parseAndExecuteCommands areaSize currentPosition (firstCommand:otherCommands) =
  let result = parseAndExecuteCommand areaSize currentPosition firstCommand in do
    if outOfArea result
      then result
      else parseAndExecuteCommands areaSize (position result) otherCommands
