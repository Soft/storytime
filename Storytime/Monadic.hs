module Storytime.Monadic where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T

import Storytime.Types
import Storytime.Evaluation

runStorytime :: Monad m => Storytime m a -> Story -> m a
runStorytime (Storytime st) s = evalStateT (runReaderT st s) initial
  where
    initial = StoryState M.empty $ start s

currentLinks :: Monad m => Storytime m [Link]
currentLinks = do
  env <- gets env
  sect <- gets section
  return $ filter (maybe True (evalExpr env) . cond) $ links sect

currentText :: Monad m => Storytime m T.Text
currentText = do
  env <- gets env
  sect <- gets section
  return $ toText env $ spans sect

performAction :: Monad m => Act -> Storytime m ()
performAction a = modify perform
  where
    perform st@(StoryState e _) = st { env = evalAct e a }

selectLink :: Monad m => Link -> Storytime m ()
selectLink l = do
  mapM_ performAction $ acts l
  sects' <- asks sects
  st <- get
  case M.lookup (target l) sects' of
   Just s -> put $ st { section = s }
   Nothing -> error "Missing section" -- I should probably include ExceptT to the stack

hasChoices :: (Functor m, Monad m) => Storytime m Bool
hasChoices = not . null <$> currentLinks

lookupMeta :: (Functor m, Monad m) => T.Text -> Storytime m (Maybe T.Text)
lookupMeta k = M.lookup k <$> asks meta


