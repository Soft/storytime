module Storytime.Monadic where

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Storytime.Types
import Storytime.Evaluation

runStorytime :: Monad m =>  StoryState -> Storytime m a -> m a
runStorytime s m = runReaderT m s

startingState :: Story -> STM StoryState
startingState s = StoryState s <$> newTVar M.empty

withStateMap :: MonadIO m => (M.Map UserID UserState -> a) -> Storytime m a
withStateMap f = asks userState >>= \st ->
  liftIO $ atomically (f <$> readTVar st)

withUserState :: MonadIO m => (UserState -> a) -> UserID -> Storytime m (Maybe a)
withUserState f u = withStateMap (\st -> f <$> M.lookup u st)

alterUserState :: MonadIO m => (Maybe UserState -> Maybe UserState) -> UserID -> Storytime m ()
alterUserState f u = asks userState >>= \st ->
  liftIO $ atomically (modifyTVar' st $ M.alter f u)
      
lookupMeta :: Monad m => T.Text -> Storytime m (Maybe T.Text)
lookupMeta k =  M.lookup k <$> asks (meta . story)
  
hasUser :: MonadIO m => UserID -> Storytime m Bool
hasUser = withStateMap . M.member

resetUser :: MonadIO m => UserID -> Storytime m ()
resetUser u = do
  s <- asks (start . story)
  let us = UserState M.empty s
  alterUserState (const $ Just us) u
  performActions u $ sectActs s

currentLinks :: MonadIO m => UserID -> Storytime m (Maybe [Link])
currentLinks = withUserState $ \st ->
  filter (maybe True (runEval $ env st) . cond) $ links . section $ st

currentText :: MonadIO m => UserID -> Storytime m (Maybe T.Text)
currentText = withUserState $ \st -> toText (env st) (spans . section $ st)

hasChoices :: MonadIO m => UserID -> Storytime m (Maybe Bool)
hasChoices u = currentLinks u >>= \st -> return $ not . null <$> st

performAction :: MonadIO m => UserID -> Act -> Storytime m ()
performAction u act = let f us = us { env = runEval (env us) act }
                      in alterUserState (fmap f) u

performActions :: (MonadIO m, Foldable t) => UserID -> t Act -> Storytime m ()
performActions u = mapM_ (performAction u) 

selectSection :: MonadIO m => UserID -> Section -> Storytime m ()
selectSection u s = let f u = u { section = s }
                    in (performActions u $ sectActs s)
                       >> alterUserState (fmap f) u

hasSection :: Monad m => Tag -> Storytime m Bool
hasSection t = M.member t <$> asks (sects . story)

selectLink :: MonadIO m => UserID -> Link -> Storytime m (Either MissingSection ())
selectLink u l = performActions u (acts l)
                 >> asks (sects . story)
                 >>= maybe
                 (return $ Left MissingSection)
                 (\s -> selectSection u s >> (return $ Right ())) . M.lookup (target l)
  
