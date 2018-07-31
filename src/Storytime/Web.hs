{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings, DeriveGeneric, DuplicateRecordFields, FlexibleContexts, TemplateHaskell #-}
module Storytime.Web (runServer) where

import Control.Monad (unless)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Loops (untilJust)
import Control.Monad.Reader (asks)
import Data.Aeson (ToJSON)
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic(..))
import Network.Wai.Handler.Warp (Port, run)
import Servant.API
import Servant.Server
import Web.FormUrlEncoded(FromForm(..), parseUnique)

import Storytime.Monadic
import Storytime.Static
import Storytime.Types

type StorytimeWeb = Storytime Handler

type StorytimeAPI = "api" :>
  ( "register" :> Post '[JSON] SessionResponse :<|>
    "meta" :> Get '[JSON] Meta :<|>
    Capture "session" UUID :> "duplicate" :> Post '[JSON] SessionResponse :<|>
    Capture "session" UUID :> "current" :> Get '[JSON] CurrentResponse :<|>
    Capture "session" UUID :> "select" :> ReqBody '[FormUrlEncoded] LinkIndex :> Post '[JSON] NoContent )

storytimeAPI = Proxy :: Proxy StorytimeAPI

type FullAPI = StorytimeAPI :<|> StaticServerAPI

fullAPI = Proxy :: Proxy FullAPI

data CurrentResponse = CurrentResponse { text :: T.Text, links :: [T.Text] }
  deriving (Show, Generic)

instance ToJSON CurrentResponse

data SessionResponse = SessionResponse { sessionID :: UUID }
  deriving (Show, Generic)

instance ToJSON SessionResponse

data LinkIndex = LinkIndex { linkIndex :: Int }
  deriving (Show, Generic)

instance FromForm LinkIndex

generateSession :: StorytimeWeb UUID
generateSession = untilJust $ do
  session <- liftIO nextRandom
  reserved <- hasUser session
  return $ if not reserved
           then Just session
           else Nothing
  
handleRegister :: StorytimeWeb SessionResponse
handleRegister = do
  session <- generateSession
  resetUser session
  return $ SessionResponse session

handleMeta :: StorytimeWeb Meta
handleMeta = asks $ meta . story

maybe404 :: MonadError ServantErr m => m (Maybe a) -> m a
maybe404 = (>>= maybe (throwError err404) return)

handleDuplicate :: UUID -> StorytimeWeb SessionResponse
handleDuplicate session = do
  new <- generateSession
  state <- maybe404 $ withUserState id session
  alterUserState (const $ Just state) new
  return $ SessionResponse new

handleCurrent :: UUID -> StorytimeWeb CurrentResponse
handleCurrent session = do
  text <- maybe404 $ currentText session
  links <- maybe404 $ currentLinks session
  return $ CurrentResponse text $ title <$> links

handleSelect :: UUID -> LinkIndex -> StorytimeWeb NoContent
handleSelect session (LinkIndex ind) = do
  links <- maybe404 $ currentLinks session
  unless (0 <= ind && ind < length links) $ throwError err400
  select <- selectLink session (links !! ind)
  either (const $ throwError err501) (const $ return NoContent) select

server :: ServerT StorytimeAPI StorytimeWeb
server = handleRegister :<|>
         handleMeta :<|>
         handleDuplicate :<|>
         handleCurrent :<|>
         handleSelect

application :: StoryState -> Application
application st = serve fullAPI
  $ (hoistServer storytimeAPI (runStorytime st) server) :<|> staticServer

runServer :: Port -> StoryState -> IO ()
runServer p st = run p $ application st

