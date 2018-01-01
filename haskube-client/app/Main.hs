{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main where

import           Control.Applicative
import           Control.Lens                  (view, (&), (.~), (?~), (^?),
                                                _Just)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe

import Data.Maybe (fromMaybe)
import qualified Data.Aeson                    as AE
import qualified Data.Aeson.Encode.Pretty      as AE
import           Data.Aeson.Lens               (key, nth, _String)
import qualified Data.Aeson.Lens               as AE
import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy.Char8    as BL
import           Data.Foldable                 (foldl')
import           Data.Map                      (Map)
import qualified Data.Map                      as M
import           Data.Monoid
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Text.Encoding            (encodeUtf8)
import qualified Data.Yaml                     as Y
import qualified Data.HashMap.Strict as HS

import           Network.Connection            (TLSSettings (..))
import           Network.HTTP.Client           (Manager, newManager)
import           Network.HTTP.Client.TLS
import           Network.Wreq
import           System.Environment            (getEnv)
import qualified Turtle                        as SH

import qualified Api.Core.V1                   as KCore
import qualified Apimachinery.Pkg.Apis.Meta.V1 as KMeta
import           Lib
import qualified KubeAuth as K
import qualified KubeConfig as K

main :: IO ()
main = do
  mAuthContext <- runMaybeT K.getDefaultAuthContext
  case mAuthContext of
    Nothing -> putStrLn "couldn't load auth context"
    Just authContext -> do
      let authHeader = K.getAuthHeader authContext
      print authHeader
      runApp (K._authHost authContext) authHeader $ do
        printJSON =<< runExceptT (postNamespace testNamespace)
        printJSON =<< runExceptT (getNamespace "haskube-test-ns")
        printJSON =<< runExceptT (deleteNamespace "haskube-test-ns")

turtleMain :: IO ()
turtleMain = do
  result <- fmap join . SH.single . runMaybeT $ script
  case result of Nothing -> print "Oh no!"
                 Just (server, token) -> do
                   print server
                   print token
                   let authHeader = oauth2Bearer $ encodeUtf8 token
                   runApp server authHeader $ do
                     printJSON =<< runExceptT (postNamespace testNamespace)
                     printJSON =<< runExceptT (getNamespace "haskube-test-ns")
                     printJSON =<< runExceptT (putNamespace "haskube-test-ns" testNamespace2)
                     printJSON =<< runExceptT (getNamespace "haskube-test-ns")
                     printJSON =<< runExceptT (deleteNamespace "haskube-test-ns")
                     printJSON =<< runExceptT (getNamespace "haskube-test-ns")

data Env = Env { _envServer  :: Text
               , _envOptions :: Options
               , _envManager :: Manager
               }

type AppM = ReaderT Env IO

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

mkOptions :: Text -> Options
mkOptions token =
  defaults & auth ?~ oauth2Bearer (encodeUtf8 token)

runApp :: Text -> Auth -> AppM a -> IO a
runApp server authHeader action = do
  let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
  man <- newManager settings
  let opts = defaults
        & auth ?~ authHeader
        & manager .~ Right man
      env = Env { _envServer = server
                , _envOptions = opts
                , _envManager = man
                }
  runReaderT action env

getPods :: ExceptT JSONError AppM (Response KCore.PodList)
getPods = do
  Env{..} <- ask
  let address = T.unpack $ _envServer <> "/api/v1/pods"
  r <- liftIO $ getWith _envOptions $ address
  asJSON r

getPodsInNamespace :: Text -> ExceptT JSONError AppM (Response KCore.PodList)
getPodsInNamespace nsName = do
  Env{..} <- ask
  let address = T.unpack $ _envServer <> "/api/v1/namespaces/" <> nsName <> "/pods"
  r <- liftIO $ getWith _envOptions $ address
  asJSON r


getNamespace :: Text -> ExceptT JSONError AppM (Response KCore.Namespace)
getNamespace nsName = do
  Env{..} <- ask
  let address = T.unpack $ _envServer <> "/api/v1/namespaces/" <> nsName
  r <- liftIO $ getWith _envOptions $ address
  asJSON r

postNamespace :: KCore.Namespace -> ExceptT JSONError AppM (Response KCore.Namespace)
postNamespace namespace = do
  Env{..} <- ask
  let address = T.unpack $ _envServer <> "/api/v1/namespaces"
  r <- liftIO $ postWith _envOptions address $ AE.toJSON namespace
  asJSON r

putNamespace :: Text -> KCore.Namespace -> ExceptT JSONError AppM (Response KCore.Namespace)
putNamespace nsName namespace = do
  Env{..} <- ask
  let address = T.unpack $ _envServer <> "/api/v1/namespaces/" <> nsName
  r <- liftIO $ putWith _envOptions address $ AE.toJSON namespace
  asJSON r

deleteNamespace :: Text -> ExceptT JSONError AppM (Response KCore.Namespace)
deleteNamespace nsName = do
  Env{..} <- ask
  let address = T.unpack $ _envServer <> "/api/v1/namespaces/" <> nsName
  r <- liftIO $ deleteWith _envOptions $ address
  asJSON r

testNamespace2 :: KCore.Namespace
testNamespace2 = testNamespace { KCore._metadata = Just metadata } :: KCore.Namespace
  where metadata = oldMeta {KMeta._labels = mkLabels $ HS.singleton "key" "value"} :: KMeta.ObjectMeta
        oldMeta = fromMaybe defaultObjectMeta $ (KCore._metadata :: (KCore.Namespace -> Maybe KMeta.ObjectMeta)) testNamespace

mkLabels :: HS.HashMap Text Text -> Maybe KMeta.ObjectMeta_labels
mkLabels = Just . KMeta.ObjectMeta_labels

testNamespace :: KCore.Namespace
testNamespace = KCore.Namespace{ _apiVersion = Just "v1"
                               , _kind = Just "Namespace"
                               , _metadata = Just metadata
                               , _spec = Nothing
                               , _status = Nothing
                               }
  where metadata = defaultObjectMeta{ KMeta._name = Just "haskube-test-ns"} :: KMeta.ObjectMeta

printJSON  :: (MonadIO m, AE.ToJSON a) => Either JSONError (Response a) -> m ()
printJSON (Left err) = liftIO $ print err
printJSON (Right result) = liftIO . BL.putStrLn . AE.encodePretty . view responseBody $ result

  {-
podsByNamespace :: KCore.PodList -> Map Text [KCore.Pod]
podsByNamespace list =
  foldl' f M.empty $ KCore._items list
  where f dict pod = case M.lookup namespace dict of
                       Just pods -> M.insert namespace (pod:pods) dict
                       Nothing   -> M.insert namespace [pod] dict
          where namespace = KMeta._namespace . KCore._metadata $ pod
-}

defaultObjectMeta :: KMeta.ObjectMeta
defaultObjectMeta = KMeta.ObjectMeta{KMeta._generateName = Nothing,
                             KMeta._annotations = Nothing,
                             KMeta._deletionTimestamp = Nothing,
                             KMeta._uid = Nothing,
                             KMeta._deletionGracePeriodSeconds = Nothing,
                             KMeta._resourceVersion = Nothing,
                             KMeta._finalizers = Nothing,
                             KMeta._namespace = Nothing,
                             KMeta._ownerReferences = Nothing,
                             KMeta._initializers = Nothing,
                             KMeta._selfLink = Nothing,
                             KMeta._name = Nothing,
                             KMeta._creationTimestamp = Nothing,
                             KMeta._clusterName = Nothing,
                             KMeta._labels = Nothing,
                             KMeta._generation = Nothing}
