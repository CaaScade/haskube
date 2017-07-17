{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Backend.Types where

import           Data.Aeson ((.:), (.=))
import qualified Data.Aeson as AE
import           Data.Text  (Text)

import qualified Apimachinery.Pkg.Apis.Meta.V1 as K
import qualified Kubernetes.Types.Base as K

-- TODO: deriving (Show)

data Password = Password
  { _password :: Text
  }

data Addresses = Addresses
  { _masterIP :: Text
  , _masterPort :: Int
  , _slaveIP :: Text
  , _slavePort :: Int
  }

data Application = Application
  { _typeMeta :: K.TypeMeta
  , _metadata :: K.ObjectMeta
  , _spec :: ApplicationSpec
  , _status :: Maybe ApplicationStatus
  }

data ApplicationSpec = ApplicationSpec
  { _scale :: Int
  , _deploymentType :: DeploymentType
  , _secretRef :: ApplicationSecretRef
  , _resourceNamespace :: Text
  }

newtype DeploymentType = DeploymentType
  { _unDeploymentType :: Text
  }

data ApplicationSecretRef = ApplicationSecretRef
  { _name :: Text
  , _key :: Text
  }

data ApplicationStatus = ApplicationStatus
  { _state :: Maybe Text
  , _message :: Maybe Text
  , _addresses :: Maybe Addresses
  }

data ApplicationList = ApplicationList
  { _typeMeta :: K.TypeMeta
  , _metadata :: K.ListMeta
  , _items  :: [Application]
  }

isolationDeployment :: DeploymentType
isolationDeployment = DeploymentType "isolation"

scalingDeployment :: DeploymentType
scalingDeployment = DeploymentType "scaling"
