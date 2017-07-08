module Kubernetes.Types.Base where

import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AE

import Data.Int (Int64)
import Data.Text (Text)

newtype Base64String = Base64String { _unBase64String :: Text }
newtype Password = Password { _unPassword :: Text }
data IntOrString = IntVal Int64 | StringVal Text
data Null = Null

instance AE.FromJSON Base64String where
  parseJSON = fmap Base64String . AE.parseJSON
instance AE.FromJSON Password where
  parseJSON = fmap Password . AE.parseJSON
instance AE.FromJSON IntOrString where
  parseJSON (AE.String string) = return $ StringVal string
  parseJSON val = IntVal <$> AE.parseJSON val
instance AE.FromJSON Null where
  parseJSON AE.Null = return Null
  parseJSON invalid = AE.typeMismatch "Null" invalid

instance AE.ToJSON Base64String where
  toJSON = AE.toJSON . _unBase64String
instance AE.ToJSON Password where
  toJSON = AE.toJSON . _unPassword
instance AE.ToJSON IntOrString where
  toJSON (IntVal val) = AE.toJSON val
  toJSON (StringVal val) = AE.toJSON val
instance AE.ToJSON Null where
  toJSON Null = AE.Null
