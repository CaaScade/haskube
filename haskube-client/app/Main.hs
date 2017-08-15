{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Applicative
import           Control.Lens              ((^?), _Just)
import           Control.Monad
import           Control.Monad.Trans.Maybe

import           Data.Aeson.Lens           (key, nth, _String)
import qualified Data.Aeson.Lens           as Y
import           Data.Monoid
import qualified Data.Text                 as T
import qualified Data.Yaml                 as Y

import           System.Environment        (getEnv)
import           Turtle

import           Lib

main :: IO ()
main = do
  result <- fmap join . single . runMaybeT $ script
  case result of Nothing -> print "Oh no!"
                 Just (server, token) -> do
                   print server
                   print token

getToken :: MonadIO m => Text -> MaybeT m Text
getToken secretId = MaybeT $ do
  getToken_ <- single . grep (prefix "token") $ inproc "kubectl" ["describe", "secret", secretId] empty
  let token = head . tail . T.words . lineToText <$> getToken_
  return token

getSecretId :: MonadIO m => MaybeT m Text
getSecretId = do
  gotSecret <- MaybeT $ single $ grep (has "default") $ inproc "kubectl" ["get", "secrets"] empty
  let secretId = head . T.words . lineToText $ gotSecret
  return secretId

getServer :: forall m. MonadIO m => MaybeT m Text
getServer = MaybeT $ do
  home <- liftIO $ getEnv "HOME"
  let file = home <> "/.kube/config"
  let l = key "clusters" . nth 0 . key "cluster" . key "server" . _String
  config_ <- liftIO . Y.decodeFile $ file :: m (Maybe Y.Value)
  return $ config_ ^? _Just . l

script :: MaybeT Shell (Text, Text)
script = do
  secretId <- getSecretId
  token <- getToken secretId
  server <- getServer
  return (server, token)
