{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gen.AST where

import           Control.Applicative
import           Control.Monad

import           Control.Lens

import qualified Data.HashMap.Strict.InsOrd as HI
import qualified Data.Swagger               as S
import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           Gen.AST.Name               (TypeName (..), keyedTypeName,
                                             referencedTypeName)

data Newtype = Newtype
  { _newtypeName        :: Text
  , _newtypeValue       :: TypeName
  , _newtypeDescription :: Maybe Text
  } deriving (Show)

data Field = Field
  { _fieldName        :: Text
  , _fieldType        :: TypeName
  , _fieldDescription :: Maybe Text
  } deriving (Show)

data Type = Type
  { _typeName        :: TypeName
  , _typeFields      :: [Field]
  , _typeDescription :: Maybe Text
  } deriving (Show)

newtype Module = Module Text

rewriteDefinitions :: S.Definitions S.Schema -> [Type]
rewriteDefinitions definitions = rewriteDefinition <$> HI.toList definitions

rewriteDefinition :: (Text, S.Schema) -> Type
rewriteDefinition (key, schema@S.Schema {..}) =
  Type
  { _typeName = typeName
  , _typeFields = []
  , _typeDescription = S._schemaDescription schema
  }
  where
    typeName = keyedTypeName key
