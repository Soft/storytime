{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Options.Applicative
import qualified Data.Text.IO as IO

import Storytime

data Args = Args { validate :: Bool
                 , interface :: Maybe String
                 , file :: FilePath }

main :: IO ()
main = execParser opts >>= handler
  where
    opts = info (helper <*> args) desc
    desc = header "storytime: System for interactive fiction" <>
           progDesc "Interpreter for interactive fiction"
    players = intercalate ", " (fst <$> playerMap)
    args = Args
          <$> switch ( long "validate" <>
                       short 'V' <>
                       help "Run consistency checks on the input file and exit." )
          <*> optional ( strOption ( long "interface" <>
                                     short 'i' <>
                                     metavar "INTERFACE" <>
                                     help ("Select interface (" ++ players ++ ")") ))
          <*> argument str ( metavar "FILE" <>
                             action "file" )

handler :: Args -> IO ()
handler Args{..} = do
  let player = fromMaybe defaultPlayer (interface >>= flip lookup playerMap)
  story <- loadStory file
  case story of
   Right st -> if validate
               then check st
               else runFromStart player st >>= either print return
   Left e -> putStrLn "Failed to parse story file:" >> print e

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
  
defaultPlayer :: Storytime IO ()
defaultPlayer = webPlayer

playerMap :: [(String, Storytime IO ())]
playerMap = [ ("web", webPlayer)
            , ("term", termPlayer) ]
