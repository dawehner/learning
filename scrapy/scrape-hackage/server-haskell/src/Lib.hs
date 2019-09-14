{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Char (toLower)
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import qualified Data.List as List
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Control.Monad.IO.Class (liftIO)

data Package = Package
  { packageName        :: String
  , packageDescription :: Maybe String
  , packageLink  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop 7} ''Package)

type PackageAPI = "packages" :> Get '[JSON] [Package]
  :<|> "packages" :> "search" :> Capture "name" String :> Get '[JSON] Package

type API = PackageAPI

getJSON :: IO (Either String [Package])
getJSON = eitherDecode <$> B.readFile "../hackage.json"

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = packagesHandler :<|> packageHandler

packagesHandler :: Handler [Package]
packagesHandler = do
  json <- liftIO getJSON
  case json of
    Left err -> throwError err500 { errBody = BLU.fromString err }
    Right xs -> return xs

packageHandler :: String -> Handler Package
packageHandler name = do
  json <- liftIO getJSON
  case json of
    Left err -> throwError err500 { errBody = BLU.fromString err }
    Right xs -> case List.find (\package -> packageName package == name) xs of
      Nothing -> throwError err403
      Just package -> return package
