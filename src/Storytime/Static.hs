{-# LANGUAGE DataKinds, TypeOperators, TemplateHaskell, MultiParamTypeClasses #-}
module Storytime.Static (StaticServerAPI, staticServerAPI, staticServer) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.FileEmbed (embedFile)
import Lucid.Base (renderBS, toHtmlRaw)
import Servant.HTML.Lucid
import Servant.API
import Servant.Server
import Servant.Server.StaticFiles
import Data.Proxy (Proxy(..))

-- It would be nice if StaticFiles would directly support defining / path.

type StaticServerAPI = Get '[HTML] RawHTML :<|> Raw

newtype RawHTML = RawHTML { unRawHTML :: B.ByteString }

instance {-# OVERLAPPING #-} MimeRender HTML RawHTML where
  mimeRender _ = BL.fromStrict . unRawHTML

staticServerAPI = Proxy :: Proxy StaticServerAPI

staticServer :: Server StaticServerAPI
staticServer = (return $ RawHTML staticIndexHTML) :<|>
               serveDirectoryEmbedded [ ("main.css", staticMainCSS)
                                      , ("storytime.js", staticStorytimeJS) ]

staticIndexHTML = $(embedFile "frontend/dist/index.html")
staticMainCSS = $(embedFile "frontend/dist/main.css")
staticStorytimeJS = $(embedFile "frontend/dist/storytime.js")


