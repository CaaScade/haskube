{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Gen.AST.Types where

import           GHC.Generics

import           Control.Applicative
import           Control.Lens        (makeLenses)

import           Data.Aeson
import qualified Data.Aeson.Types    as A
import           Data.Maybe          (maybeToList)
import           Data.Monoid
import           Data.Text           (Text)

data TypeName where
  ArrayName :: TypeName -> TypeName
  TupleName :: [TypeName] -> TypeName
  DictionaryName :: TypeName -> TypeName
  MaybeName :: TypeName -> TypeName
  SimpleName :: Maybe Text -> Text -> TypeName
  deriving (Show)

data ExternalTypeName = ExternalName
  { _externalModule :: Text
  , _externalName   :: Text } deriving (Show, Generic)

data AdditionalProperties = AdditionalProperties deriving (Show, Generic)

data Newtype = Newtype
  { _newtypeName        :: ExternalTypeName
  , _newtypeValue       :: TypeName
  , _newtypeDescription :: Maybe Text
  } deriving (Show, Generic)

data AddlFields = AddlFields
  { _addlFieldsType        :: TypeName -- ^ The value type of each "additional field"
  , _addlFieldsDescription :: Maybe Text
  } deriving (Show, Generic)

data Field = Field
  { _fieldName        :: Text
  , _fieldType        :: TypeName
  , _fieldDescription :: Maybe Text
  } deriving (Show, Generic)

data Data = Data
  { _dataName        :: ExternalTypeName
  , _dataFields      :: [Field]
  , _dataAddlFields  :: Maybe AddlFields
  , _dataDescription :: Maybe Text
  } deriving (Show, Generic)

type Type = Either Newtype Data

makeLenses ''ExternalTypeName
makeLenses ''Newtype
makeLenses ''AddlFields
makeLenses ''Field
makeLenses ''Data

instance ToJSON TypeName where
  toJSON (ArrayName typeName) = object ["_array" .= (), "_type" .= typeName]
  toJSON (TupleName typeNames) = object ["_tuple" .= (), "_types" .= typeNames]
  toJSON (DictionaryName typeName) =
    object ["_dictionary" .= (), "_type" .= typeName]
  toJSON (MaybeName typeName) = object ["_maybe" .= (), "_type" .= typeName]
  toJSON (SimpleName moduleName typeName) =
    object ["_simple" .= (), "_module" .= moduleName, "_type" .= typeName]

instance FromJSON TypeName where
  parseJSON (Object v) =
    parseArrayName v <|> parseTupleName v <|> parseDictionaryName v <|>
    parseMaybeName v <|>
    parseSimpleName v

instance ToJSON ExternalTypeName where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON ExternalTypeName
instance ToJSON AdditionalProperties where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON AdditionalProperties
instance ToJSON Newtype where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Newtype
instance ToJSON AddlFields where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON AddlFields
instance ToJSON Field where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Field
instance ToJSON Data where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Data

parseArrayName :: Object -> A.Parser TypeName
parseArrayName v = (v .: "_array" :: A.Parser ()) >> ArrayName <$> v .: "_type"

parseTupleName :: Object -> A.Parser TypeName
parseTupleName v = (v .: "_tuple" :: A.Parser ()) >> TupleName <$> v .: "_types"

parseDictionaryName :: Object -> A.Parser TypeName
parseDictionaryName v =
  (v .: "_dictionary" :: A.Parser ()) >> DictionaryName <$> v .: "_type"

parseMaybeName :: Object -> A.Parser TypeName
parseMaybeName v = (v .: "_maybe" :: A.Parser ()) >> MaybeName <$> v .: "_type"

parseSimpleName :: Object -> A.Parser TypeName
parseSimpleName v =
  (v .: "_simple" :: A.Parser ()) >>
  SimpleName <$> v .: "_module" <*> v .: "_type"

fromExternalTypeName :: ExternalTypeName -> TypeName
fromExternalTypeName ExternalName{..} = SimpleName (Just _externalModule) _externalName

_typeModule :: Type -> Text
_typeModule (Left Newtype{..}) = _externalModule _newtypeName
_typeModule (Right Data{..})   = _externalModule _dataName

_typeNameModules :: TypeName -> [Text]
_typeNameModules (ArrayName t) = _typeNameModules t
_typeNameModules (TupleName ts) = foldl1 (<>) $ _typeNameModules <$> ts
_typeNameModules (DictionaryName t) = "Data.HashMap.Strict":_typeNameModules t
_typeNameModules (MaybeName t) = _typeNameModules t
_typeNameModules (SimpleName moduleName _) = maybeToList moduleName

data Optional a = Optional a | Required a

instance Functor Optional where
  fmap f (Optional x) = Optional (f x)
  fmap f (Required x) = Required (f x)

unOptional :: Optional a -> a
unOptional (Optional x) = x
unOptional (Required x) = x

degradeOptional :: Optional a -> Optional a
degradeOptional (Required x) = Optional x
degradeOptional x            = x
