module Lib ( 
  parseCommand,
  Command(..)
) where

data Command = 
    TurnLeft
  | TurnRight
  | MoveForward

parseCommand :: String -> Maybe Command
parseCommand str = Nothing
