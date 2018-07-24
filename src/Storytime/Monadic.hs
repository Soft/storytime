module Storytime.Monadic where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Storytime.Types
import Storytime.Evaluation

runStorytime :: Monad m => Storytime m a -> Story -> StoryState -> m (Either StoryError a, StoryState)
runStorytime m s = runStateT (runReaderT (runExceptT $ unStorytime m) s)

runFromStart :: Monad m => Storytime m a -> Story -> m (Either StoryError a)
runFromStart m s = liftM fst $ runStorytime m' s (startingState s)
  where
    m' = selectSection (start s) >> m

startingState :: Story -> StoryState
startingState = StoryState M.empty . start

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

selectSection :: Monad m => Section -> Storytime m ()
selectSection s = do
  mapM_ performAction $ sectActs s
  st <- get
  put $ st { section = s }

performAction :: Monad m => Act -> Storytime m ()
performAction a = modify perform
  where
    perform st@(StoryState e _) = st { env = runEval e a }

selectLink :: Monad m => Link -> Storytime m ()
selectLink l = do
  mapM_ performAction $ acts l
  sects' <- asks sects
  let t = target l
  case M.lookup t sects' of
   Just s -> selectSection s
   Nothing -> throwError $ MissingSection t

hasChoices :: (Functor m, Monad m) => Storytime m Bool
hasChoices = not . null <$> currentLinks

lookupMeta :: (Functor m, Monad m) => T.Text -> Storytime m (Maybe T.Text)
lookupMeta k = M.lookup k <$> asks meta


