{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Storytime.Web (webPlayer) where

import Prelude hiding (error)
import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Reader hiding (local)
import Control.Monad.State
import Data.Aeson hiding (json)
import Data.List
import Data.Maybe
import GHC.Generics
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Local
import Network.Wai.Middleware.Static
import Network.Wai.Parse
import System.Process (rawSystem)
import Text.Read (readMaybe)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T

import Storytime.Monadic
import Storytime.Types

data View = View { text :: T.Text, links :: [T.Text] }
          deriving (Show, Generic)

instance ToJSON View

infixl 9 !?

(!?) :: [a] -> Int -> Maybe a
[]     !? i = Nothing
(x:_)  !? 0 = Just x
(x:xs) !? n | n < 0 = Nothing
            | otherwise = xs !? pred n

getView :: (Functor m, Monad m) => Storytime m View
getView = View <$> currentText <*> (fmap title <$> currentLinks)

port :: Port
port = 8080

handleCurrent :: Request -> Storytime IO Response
handleCurrent _ = json status200 <$> getView

handleMeta :: Request -> Storytime IO Response
handleMeta _ = json status200 <$> asks meta

handleSelect :: Request -> Storytime IO Response
handleSelect req = do
  (params, _) <- liftIO $ parseRequestBody lbsBackEnd req
  links <- currentLinks
  let query = C8.unpack <$> lookup "id" params >>= readMaybe >>= (links !?)
  maybe invalid (selectLink >=> const ok)  query
  where
    ok = return $ responseLBS status200 [] ""
    invalid = return $ error "invalid link"

error :: String -> Response
error er = json status500 $ object ["error" .= er]

handleError :: StoryError -> Response
handleError = error . show

json :: ToJSON a => Status -> a -> Response
json s = responseLBS s [(hContentType, "application/json")] . encode

app :: Request -> Storytime IO Response
app req = route defs notFound req req
  where
    defs = [ (methodGet,  ["current"], handleCurrent)
           , (methodGet,  ["meta"],    handleMeta)
           , (methodPost, ["select"],  handleSelect) ]
    notFound _ = return $ responseLBS status404 [] ""

route :: [(Method, [T.Text], a)] -> a -> Request -> a
route r d req = fromMaybe d $ third <$> find match r
  where
    match (m, p, _) = requestMethod req == m && pathInfo req == p
    third (_, _, c) = c

appMaker :: Storytime IO ((Request -> Storytime IO Response) -> Application)
appMaker = do
  story <- ask
  state <- get
  var <- liftIO $ atomically $ newTVar state
  return $ toApplication story var

toApplication :: Story -> TVar StoryState -> (Request -> Storytime IO Response) -> Application
toApplication s var f req send = do
  st <- readTVarIO var
  (e, st') <- runStorytime (f req) s st
  atomically $ swapTVar var st'
  case e of
   Right resp -> send resp
   Left er -> send $ handleError er

launchBrowser :: (Functor m, MonadIO m) => m ()
launchBrowser = liftIO . void . forkIO $ do
  threadDelay wait
  void $ rawSystem "xdg-open" ["http://127.0.0.1:" ++ show port]
  where
    wait = 1 * 10^6

webPlayer :: Storytime IO ()
webPlayer = do
  story <- ask
  make <- appMaker
  prompt
  launchBrowser
  liftIO $ run port $ local forbidden $ make app
  where
    prompt = liftIO . putStrLn $ "And the story begins (listening on port " ++ show port ++ ")"
    forbidden = responseLBS status403 [] ""

