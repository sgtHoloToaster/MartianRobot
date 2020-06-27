module Lib ( 
  parseCommand,
  Command(..)
) where

data Command = 
    TurnLeft
  | TurnRight
  | MoveForward 
  deriving (Show, Eq)

parseCommand :: String -> Maybe Command
parseCommand "L" = Just TurnLeft
parseCommand "R" = Just TurnRight
parseCommand "F" = Just MoveForward
parseCommand _ = Nothing