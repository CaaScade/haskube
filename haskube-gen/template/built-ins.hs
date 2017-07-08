module Kubernetes.Types.Base where

import qualified Data.Aeson as AE

import Data.Int (Int64)
import Data.Text (Text)

newtype Base64String = Base64String { unBase64String :: Text }
newtype Password = Password { unPassword :: Text }
data IntOrString = IntVal Int64 | StringVal Text
data Null = Null

instance AE.FromJSON Base64String where
  parseJSON = fmap Base64String . AE.parseJSON
instance AE.FromJSON Password where
  parseJSON = fmap Password . AE.parseJSON
instance AE.FromJSON IntOrString where
  parseJSON (AE.String string) = return $ StringVal string
  parseJSON val = IntVal <$> AE.parseJSON val
