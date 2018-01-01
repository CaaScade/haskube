{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module KubeConfig where

import           Control.Lens              (Getting, makeLenses, view, (&),
                                            (.~), (?~), (^?), _Just)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe

import qualified Data.Aeson                as AE
import           Data.Aeson.Lens           (key, nth, _Array, _Bool, _String)
import qualified Data.Aeson.Lens           as AE
import           Data.Maybe
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Vector               as V
import qualified Data.Yaml                 as Y

import           System.Environment        (getEnv)

-- kubeconfig type is defined at k8s.io/client-go/tools/clientcmd/api/v1/types.go

readDefaultKubeConfig :: forall m. MonadIO m => MaybeT m Y.Value
readDefaultKubeConfig = MaybeT $ do
  home <- liftIO $ getEnv "HOME"
  let file = home <> "/.kube/config"
  liftIO . Y.decodeFile $ file

previewCurrentContext :: Y.Value -> Maybe Y.Value
previewCurrentContext config = do
  contextName <- config ^? key "current-context" . _String
  context <-
    previewNamedItem
      config
      (key "contexts" . _Array)
      contextName
      (key "name" . _String)
  context ^? key "context"

previewCurrentUserForContext :: Y.Value -> Y.Value -> Maybe Y.Value
previewCurrentUserForContext context config = do
  userName <- context ^? key "user" . _String
  user <- previewNamedItem config (key "users" . _Array) userName (key "name" . _String)
  user ^? key "user"

previewCurrentClusterForContext :: Y.Value -> Y.Value -> Maybe Y.Value
previewCurrentClusterForContext context config = do
  clusterName <- context ^? key "cluster" . _String
  cluster <-
    previewNamedItem
      config
      (key "clusters" . _Array)
      clusterName
      (key "name" . _String)
  cluster ^? key "cluster"

previewNamedItem
  :: Eq t =>
     s
     -> Getting (First (V.Vector b)) s (V.Vector b)
     -> t
     -> Getting (First t) b t
     -> Maybe b
previewNamedItem whole parts name partName = do
  parts_ <- whole ^? parts
  maybeHead . V.toList . fmap namedPart $ parts_
  where namedPart part_ = case part_ ^? partName of
          Nothing -> Nothing
          Just n  -> if n == name then Just part_ else Nothing

maybeHead :: [Maybe a] -> Maybe a
maybeHead = listToMaybe . catMaybes

data AuthContext = AuthContext
  { _authTLS          :: TLSContext
  , _authUserPassword :: UserPassword
  , _authHost         :: Text
  } deriving (Show, Eq)

data TLSContext = TLSContext
  { _tlsSkipVerify    :: Bool
  , _tlsClientCert    :: Text
  , _tlsClientKey     :: Text
  , _tlsCertAuthority :: Text
  } deriving (Show, Eq)

data UserPassword = UserPassword
  { _upUsername     :: Text
  , _upPassword :: Text
  } deriving (Show, Eq)

makeLenses ''AuthContext
makeLenses ''TLSContext
makeLenses ''UserPassword

getUserPassword :: Y.Value -> Maybe UserPassword
getUserPassword user = do
  username <- user ^? key "username" . _String
  password <- user ^? key "password" . _String
  return UserPassword{ _upUsername = username, _upPassword = password }

getTLSContext :: Y.Value -> Y.Value -> Maybe (Text, TLSContext)
getTLSContext cluster user = do
  host <- cluster ^? key "server" . _String
  let isHttps = T.isPrefixOf "https" host
  guard isHttps
  certAuthority <- cluster ^? key "certificate-authority-data" . _String
  clientCert <- user ^? key "client-certificate-data" . _String
  clientKey <- user ^? key "client-key-data" . _String
  let skipVerify =
        fromMaybe False $ cluster ^? key "insecure-skip-tls-verify" . _Bool
      context =
        TLSContext
        { _tlsSkipVerify = skipVerify
        , _tlsClientCert = clientCert
        , _tlsClientKey = clientKey
        , _tlsCertAuthority = certAuthority
        }
  return (host, context)

getAuthContext :: Y.Value -> Maybe AuthContext
getAuthContext config = do
  context <- previewCurrentContext config
  user <- previewCurrentUserForContext context config
  cluster <- previewCurrentClusterForContext context config
  (host, tlsContext) <- getTLSContext cluster user
  userPassword <- getUserPassword user
  return
    AuthContext
    {_authTLS = tlsContext, _authUserPassword = userPassword, _authHost = host}

getDefaultAuthContext :: MaybeT IO AuthContext
getDefaultAuthContext = do
    config <- readDefaultKubeConfig
    MaybeT . return $ getAuthContext config
