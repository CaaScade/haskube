{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Gen.AST.Types where

import           Control.Lens (makeLenses)

import           Data.Text    (Text)

data TypeName where
  ArrayName :: TypeName -> TypeName
  TupleName :: [TypeName] -> TypeName
  DictionaryName :: TypeName -> TypeName
  SimpleName :: Maybe Text -> Text -> TypeName
  deriving (Show)

data ExternalTypeName = ExternalName
  { _externalModule :: Maybe Text
  , _externalName   :: Text } deriving (Show)

data AdditionalProperties = AdditionalProperties deriving (Show)

data Newtype = Newtype
  { _newtypeName        :: ExternalTypeName
  , _newtypeValue       :: TypeName
  , _newtypeDescription :: Maybe Text
  } deriving (Show)

data Field = Field
  { _fieldName        :: Either AdditionalProperties Text
  , _fieldType        :: TypeName
  , _fieldDescription :: Maybe Text
  } deriving (Show)

data Data = Data
  { _dataName        :: TypeName
  , _dataFields      :: [Field]
  , _dataDescription :: Maybe Text
  } deriving (Show)

type Type = Either Newtype Data

makeLenses ''ExternalTypeName
makeLenses ''Newtype
makeLenses ''Field
makeLenses ''Data

fromExternalTypeName :: ExternalTypeName -> TypeName
fromExternalTypeName ExternalName{..} = SimpleName _externalModule _externalName

data Optional a = Optional a | Required a

instance Functor Optional where
  fmap f (Optional x) = Optional (f x)
  fmap f (Required x) = Required (f x)

unOptional :: Optional a -> a
unOptional (Optional x) = x
unOptional (Required x) = x
