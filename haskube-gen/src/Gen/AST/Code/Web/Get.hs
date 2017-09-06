{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Gen.AST.Code.Web.Get where

import           Language.Haskell.Exts

import qualified Data.HashMap.Strict.InsOrd as HI
import           Data.Monoid
import           Data.Text                  (Text, toTitle)

import           Gen.AST.BuiltIn
import           Gen.AST.Code.Combinators
import           Gen.AST.Code.Types
import           Gen.AST.Types              (Data (..), ExternalTypeName (..),
                                             Field (..), TypeName (..))
import qualified Gen.AST.Types              as G
import           Gen.AST.Web.Get

wreqPrefix :: Text
wreqPrefix = "Wreq"

wreqImport :: ImportDecl Ann
wreqImport =
  ImportDecl
  { importAnn = mempty
  , importModule = mkModuleName "Network.Wreq"
  , importQualified = True
  , importSrc = False
  , importSafe = False
  , importPkg = Nothing
  , importAs = Just $ mkModuleName wreqPrefix
  , importSpecs = Nothing
  }

getRequestType :: Get -> Data
getRequestType Get {..} =
  Data
  { _dataName = requestName _getPathTemplate
  , _dataFields = queryFields _getQueryParams <> pathFields _getPathParams
  , _dataAddlFields = Nothing
  , _dataDescription = _getDescription
  }

requestName :: Text -- ^ path template
            -> ExternalTypeName
requestName = _

queryFields :: Params QueryParam -> [Field]
queryFields = paramFields "query" queryFieldType

queryFieldType :: QueryParamType -> TypeName
queryFieldType QueryParamBool = boolName
queryFieldType QueryParamInt = int64Name
queryFieldType QueryParamString = textName

pathFields :: Params PathParam -> [Field]
pathFields = paramFields "path" $ const textName

paramFields :: forall t. Text -- ^ field name prefix
            -> (t -> TypeName)
            -> Params (Param t) -> [Field]
paramFields namePrefix convertType = fmap (uncurry paramField) . HI.toList
  where
    paramField :: Text -> Param t -> Field
    paramField name Param {..} =
      Field
      { _fieldName = paramFieldName name
      , _fieldType = convertType _paramType
      , _fieldDescription = _paramDescription
      , _fieldRequired = _paramRequired
      }
    paramFieldName :: Text -> Text
    paramFieldName = mappend namePrefix . toTitle
