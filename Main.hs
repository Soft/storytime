module Main where

import System.Environment

import Storytime

main :: IO ()
main = do
  file:_ <- getArgs
  story <- loadStory file
  case story of
   Right st -> runStorytime termPlayer st
   Left e -> do
     putStrLn "Failed to parse story file: "
     print e
