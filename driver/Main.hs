{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent (forkOS, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Monad (mfilter, when, unless, void)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Text.IO as IO
import Network.Wai.Handler.Warp (Port)
import Options.Applicative
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)
import Web.Browser (openBrowser)

import Storytime

data Args = Args { validate :: Bool
                 , open :: Bool
                 , port :: Port
                 , file :: FilePath }

main :: IO ()
main = execParser opts >>= handler
  where
    opts = info (helper <*> args) desc
    desc = header "storytime: System for interactive fiction" <>
           progDesc "Interpreter for interactive fiction"
    validPort = mfilter (> 1024) . readMaybe
    args = Args
          <$> switch ( long "validate" <>
                       short 'V' <>
                       help "Run consistency checks on the input file and exit." )
          <*> switch ( long "open" <>
                       short 'o' <>
                       help "Open browser." )
          <*> option (maybeReader validPort)
          ( long "port" <>
            short 'p' <>
            metavar "PORT" <>
            value 8080 <>
            help "Server port." )
          <*> argument str
          ( metavar "FILE" <>
            action "file" )

handler :: Args -> IO ()
handler Args{..} = do
  story <- loadStory file
  let addr = serverURL port
  case story of
   Right st -> if validate
     then check st
     else do
     start <- atomically $ startingState st
     when open $ openBrowserThread addr
     putStrLn $ "Starting Storytime server at " ++ addr
     runServer port start
   Left e -> putStrLnErr $ "Failed to parse story file:" ++ show e

openBrowserThread :: String -> IO ()
openBrowserThread address = void . forkOS $ do
  threadDelay $ 3 * 10^6
  result <- openBrowser address
  unless result (putStrLnErr "Failed to open browser")

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

serverURL :: Port -> String
serverURL port = "http://127.0.0.1:" ++ show port

putStrLnErr :: String -> IO ()
putStrLnErr = hPutStrLn stderr

