module Lib ( 
    parseCommand
  , parseCommands
  , Command(..)
  , Direction(..)
  , turnLeft
  , turnRight
  , moveForward
  , Position(..)
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