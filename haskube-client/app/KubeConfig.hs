{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module KubeConfig where

import           Control.Lens              (Getting, view, (&), (.~), (?~),
                                            (^?), _Just)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe

import qualified Data.Aeson                as AE
import           Data.Aeson.Lens           (key, nth, _Array, _String)
import qualified Data.Aeson.Lens           as AE
import           Data.Maybe
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Vector               as V
import qualified Data.Yaml                 as Y

import           System.Environment        (getEnv)

readDefaultKubeConfig :: forall m. MonadIO m => MaybeT m Y.Value
readDefaultKubeConfig = MaybeT $ do
  home <- liftIO $ getEnv "HOME"
  let file = home <> "/.kube/config"
  liftIO . Y.decodeFile $ file

previewCurrentContext :: Y.Value -> Maybe Y.Value
previewCurrentContext config = do
  contextName <- config ^? key "current-context" . _String
  context <- previewNamedItem config (key "contexts" . _Array) contextName (key "name" . _String)
  context ^? key "context"

previewCurrentUserForContext :: Y.Value -> Y.Value -> Maybe Y.Value
previewCurrentUserForContext context config = do
  userName <- context ^? key "user" . _String
  user <- previewNamedItem config (key "users" . _Array) userName (key "name" . _String)
  user ^? key "user"

previewCurrentClusterForContext :: Y.Value -> Y.Value -> Maybe Y.Value
previewCurrentClusterForContext context config = do
  clusterName <- context ^? key "cluster" . _String
  cluster <- previewNamedItem config (key "clusters" . _Array) clusterName (key "name" . _String)
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

test :: IO (Maybe (Y.Value, Y.Value))
test = runMaybeT $ do
  config <- readDefaultKubeConfig
  MaybeT . return $ do
    context <- previewCurrentContext config
    user <- previewCurrentUserForContext context config
    cluster <- previewCurrentClusterForContext context config
    return (user, cluster)
