module Storytime.Monadic where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Storytime.Types
import Storytime.Evaluation

runStorytime :: Monad m => Storytime m a -> Story -> m (Either StoryError a)
runStorytime (Storytime st) s = evalStateT (runReaderT (runExceptT st) s) initial
  where
    initial = StoryState M.empty $ start s

currentLinks :: Monad m => Storytime m [Link]
currentLinks = do
  env <- gets env
  sect <- gets section
  return $ filter (maybe True (runEval env) . cond) $ links sect

currentText :: Monad m => Storytime m T.Text
currentText = do
  env <- gets env
  sect <- gets section
  return $ toText env $ spans sect

performAction :: Monad m => Act -> Storytime m ()
performAction a = modify perform
  where
    perform st@(StoryState e _) = st { env = runEval e a }

selectLink :: Monad m => Link -> Storytime m ()
selectLink l = do
  mapM_ performAction $ acts l
  sects' <- asks sects
  st <- get
  let t = target l
  case M.lookup t sects' of
   Just s -> put $ st { section = s }
   Nothing -> throwError $ MissingSection t

hasChoices :: (Functor m, Monad m) => Storytime m Bool
hasChoices = not . null <$> currentLinks

lookupMeta :: (Functor m, Monad m) => T.Text -> Storytime m (Maybe T.Text)
lookupMeta k = M.lookup k <$> asks meta


