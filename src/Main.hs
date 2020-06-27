module Main where

data Command =
   TurnLeft
 | TurnRight
 | MoveForward

parseCommand :: String -> Command
parseCommand str = TurnLeft

main :: IO ()
main = putStrLn "Hello, Haskell!"
