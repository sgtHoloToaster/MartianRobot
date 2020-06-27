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
parseCommand str = Nothing