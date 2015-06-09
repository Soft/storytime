{-# LANGUAGE OverloadedStrings #-}
module Storytime.Terminal (termPlayer) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid ((<>))
import Storytime.Monadic
import Text.Read (readMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as IO

import Storytime.Types

termPlayer :: Storytime IO ()
termPlayer = do
  text <- currentText
  links <- currentLinks
  liftIO $ IO.putStrLn text
  unless (null links) $ do
    mapM_ putLink $ zip [1..] links
    answer <- liftIO getLine
    case readMaybe answer of
     Just n -> selectLink (links !! pred n)
     Nothing -> liftIO $ putStrLn "Not a valid choice"
    termPlayer
  where
    putLink (i, l) = liftIO $ IO.putStrLn $ T.pack (show i) <> " " <> title l

