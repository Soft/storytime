module Main where

import Control.Monad
import Options.Applicative
import qualified Data.Text.IO as IO

import Storytime

main :: IO ()
main = join $ execParser opts
  where
    opts = info (helper <*> cmd) desc
    desc = header "storytime: System for interactive fiction" <>
           progDesc "Interpreter for interactive fiction"
    cmd = handler
          <$> switch ( long "validate" <>
                       short 'V' <>
                       help "Run consistency checks on the input file and exit." )
          <*> argument str ( metavar "FILE" <>
                             action "file" )

handler :: Bool -> FilePath -> IO ()
handler validate file = do
  story <- loadStory file
  case story of
   Right st -> if validate
               then check st
               else runStorytime termPlayer st
   Left e -> do
     putStrLn "Failed to parse story file:"
     print e

check :: Story -> IO ()
check story = do
  unless (null missing) $ do
    putStrLn "Missing targets:"
    mapM_ IO.putStrLn missing
  unless (null orphans') $ do
    putStrLn "Orphan sections:"
    mapM_ IO.putStrLn orphans'
  where
    missing = missingTargets story
    orphans' = orphans story
  
