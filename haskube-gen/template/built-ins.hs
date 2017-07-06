module Kubernetes.Types.Base where

import Data.Int (Int64)
import Data.Text (Text)

newtype Base64String = Base64String { unBase64String :: Text }
newtype Password = Password { unPassword :: Text }
data IntOrString = IntVal Int64 | StringVal Text
data Null = Null
