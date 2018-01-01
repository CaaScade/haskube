{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module KubeTurtle where

import           Control.Applicative
import           Control.Lens              ((^?), _Just)
import           Control.Monad

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe

import           Data.Aeson.Lens           (key, nth, _String)
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Yaml                 as Y
import           System.Environment        (getEnv)

import qualified Turtle                    as SH


getToken :: MonadIO m => Text -> MaybeT m Text
getToken secretId = MaybeT $ do
  getToken_ <- SH.single . SH.grep (SH.prefix "token") $ SH.inproc "kubectl" ["describe", "secret", secretId] empty
  let token = head . tail . T.words . SH.lineToText <$> getToken_
  return token

getSecretId :: MonadIO m => MaybeT m Text
getSecretId = do
  gotSecret <- MaybeT $ SH.single $ SH.grep (SH.has "default") $ SH.inproc "kubectl" ["get", "secrets"] empty
  let secretId = head . T.words . SH.lineToText $ gotSecret
  return secretId

getServer :: forall m. MonadIO m => MaybeT m Text
getServer = MaybeT $ do
  home <- liftIO $ getEnv "HOME"
  let file = home <> "/.kube/config"
  let l = key "clusters" . nth 0 . key "cluster" . key "server" . _String
  config_ <- liftIO . Y.decodeFile $ file :: m (Maybe Y.Value)
  return $ config_ ^? _Just . l

script :: MaybeT SH.Shell (Text, Text)
script = do
  secretId <- getSecretId
  token <- getToken secretId
  server <- getServer
  return (server, token)

getServerToken :: IO (Maybe (Text, Text))
getServerToken = fmap join . SH.single . runMaybeT $ script
