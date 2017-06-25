{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Gen.AST where

import           Control.Applicative
import           Control.Monad

import           Control.Lens

import qualified Data.HashMap.Strict.InsOrd as HI
import           Data.Monoid
import qualified Data.Swagger               as S
import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           Gen.AST.Name               (TypeName (..), arrayTypeName,
                                             boolTypeName, integerTypeName,
                                             keyedTypeName, numberTypeName,
                                             referencedTypeName, stringTypeName)

data Newtype = Newtype
  { _newtypeName        :: TypeName
  , _newtypeValue       :: TypeName
  , _newtypeDescription :: Maybe Text
  } deriving (Show)

data Field = Field
  { _fieldName        :: Text
  , _fieldType        :: TypeName
  , _fieldDescription :: Maybe Text
  } deriving (Show)

data Data = Data
  { _dataName        :: TypeName
  , _dataFields      :: [Field]
  , _dataDescription :: Maybe Text
  } deriving (Show)

type Type = Either Newtype Data

newtype Module = Module Text

mkNewtype :: S.Schema -> TypeName -> Newtype
mkNewtype schema typeName =
  Newtype
  { _newtypeName = typeName
  , _newtypeValue = stringTypeName
  , _newtypeDescription = S._schemaDescription schema
  }

rewriteDefinitions :: S.Definitions S.Schema -> [Type]
rewriteDefinitions definitions = rewriteDefinition <$> HI.toList definitions

-- TODO: http://swagger.io/specification/#dataTypeFormat
rewriteDefinition :: (Text, S.Schema) -> Type
rewriteDefinition (key, schema) =
  case S._paramSchemaType . S._schemaParamSchema $ schema of
    S.SwaggerString  -> Left $ mkNewtype schema stringTypeName
    S.SwaggerNumber  -> Left $ mkNewtype schema numberTypeName
    S.SwaggerInteger -> Left $ mkNewtype schema integerTypeName
    S.SwaggerBoolean -> Left $ mkNewtype schema boolTypeName
    S.SwaggerArray   -> _
    S.SwaggerObject  -> _
  where
    typeName = keyedTypeName key

rewriteArrayDefinition :: TypeName -> S.Schema -> TypeName
rewriteArrayDefinition typeName arraySchema =
  case S._paramSchemaItems . S._schemaParamSchema $ arraySchema of
    Nothing -> error $ "arraySchema should have paramSchemaItems: " <> show arraySchema
    Just (S.SwaggerItemsPrimitive collectionFormat_ itemParamSchema) ->
      _
    Just (S.SwaggerItemsObject ref) -> arrayTypeName $ referencedTypeName ref
