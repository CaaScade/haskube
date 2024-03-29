{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Kubernetes.Types.Base where

import           Control.Monad

import qualified Data.Aeson          as AE
import qualified Data.Aeson.Types    as AE
import qualified Data.HashMap.Lazy   as HL
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet        as HS

import           Data.Int            (Int64)
import           Data.Monoid
import           Data.Text           (Text)

newtype Base64String = Base64String { _unBase64String :: Text } deriving (Show, Eq)
newtype Password = Password { _unPassword :: Text } deriving (Show, Eq)
data IntOrString = IntVal Int64 | StringVal Text deriving (Show, Eq)
data Null = Null deriving (Show, Eq)
data TypeMeta = TypeMeta { _kind :: Maybe Text, _apiVersion :: Maybe Text } deriving (Show, Eq)

instance AE.FromJSON Base64String where
  parseJSON = fmap Base64String . AE.parseJSON
instance AE.FromJSON Password where
  parseJSON = fmap Password . AE.parseJSON
instance AE.FromJSON IntOrString where
  parseJSON (AE.String string) = return $ StringVal string
  parseJSON val                = IntVal <$> AE.parseJSON val
instance AE.FromJSON Null where
  parseJSON AE.Null = return Null
  parseJSON invalid = AE.typeMismatch "Null" invalid
instance AE.FromJSON TypeMeta where
  parseJSON (AE.Object obj) = TypeMeta <$> obj AE..: "kind" <*> obj AE..: "apiVersion"
  parseJSON invalid = AE.typeMismatch "TypeMeta" invalid

instance AE.ToJSON Base64String where
  toJSON = AE.toJSON . _unBase64String
instance AE.ToJSON Password where
  toJSON = AE.toJSON . _unPassword
instance AE.ToJSON IntOrString where
  toJSON (IntVal val)    = AE.toJSON val
  toJSON (StringVal val) = AE.toJSON val
instance AE.ToJSON Null where
  toJSON Null = AE.Null
instance AE.ToJSON TypeMeta where
  toJSON val = addTypeMeta val $ AE.Object H.empty

justPair :: (AE.ToJSON a, AE.KeyValue kv) => Text -> a -> Maybe kv
justPair key value = Just $ key AE..= value

maybePair :: (AE.ToJSON a, AE.KeyValue kv) => Text -> Maybe a -> Maybe kv
maybePair key = fmap $ \value -> key AE..= value

parseAddlProps
  :: forall a.
     (AE.FromJSON a)
  => [Text] -> AE.Object -> AE.Parser (H.HashMap Text a)
parseAddlProps ignoredProps_ obj =
  foldM f H.empty . filter isNotIgnored $ HL.toList obj
  where ignoredProps = HS.fromList ignoredProps_
        isNotIgnored (prop, _) = not $ prop `HS.member` ignoredProps
        f :: H.HashMap Text a -> (Text, AE.Value) -> AE.Parser (H.HashMap Text a)
        f addlProps (prop, val_) = do
          val <- AE.parseJSON val_
          return $ H.insert prop val addlProps

addAddlProps :: forall a. (AE.ToJSON a) => H.HashMap Text a -> AE.Value -> AE.Value
addAddlProps addlProps (AE.Object props0) =
  AE.Object $ H.foldlWithKey' f props0 addlProps
  where f :: HL.HashMap Text AE.Value -> Text -> a -> HL.HashMap Text AE.Value
        f props prop val = HL.insert prop (AE.toJSON val) props
addAddlProps _ _ = error "addAddlProps should only be called on JSON \"Object\"s"

addTypeMeta :: TypeMeta -> AE.Value -> AE.Value
addTypeMeta TypeMeta{..} (AE.Object obj) =
  AE.Object . (addProp "kind" _kind) . (addProp "apiVersion" _apiVersion) $  obj

addProp :: AE.ToJSON a => Text -> a -> H.HashMap Text AE.Value -> H.HashMap Text AE.Value
addProp prop val = H.insert prop $ AE.toJSON val
