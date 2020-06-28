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

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft West = South
turnLeft South = East
turnLeft East = North

turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

type Coordinates = (Int, Int)
data Position = Position {
    coordinates :: Coordinates
  , direction :: Direction
}

moveForward :: Position -> Coordinates
moveForward position = (0, 0)