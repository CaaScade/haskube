{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Gen.AST.Types where

import Control.Lens (makeLenses)

import Data.Text (Text)

data TypeName where
  ArrayName :: TypeName -> TypeName
  TupleName :: [TypeName] -> TypeName
  DictionaryName :: TypeName -> TypeName
  SimpleName :: Maybe Text -> Text -> TypeName
  deriving (Show)

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
