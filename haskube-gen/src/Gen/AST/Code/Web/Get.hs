{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Gen.AST.Code.Web.Get where

import           Language.Haskell.Exts

import           Control.Monad.Except
import           Control.Monad.Reader

import qualified Data.HashMap.Strict.InsOrd as HI
import           Data.Monoid
import           Data.Text                  (Text, toTitle)
import           Text.Parsec                (ParseError, runParser)

import           Gen.AST.BuiltIn
import           Gen.AST.Code.Combinators
import           Gen.AST.Code.Types
import           Gen.AST.Code.Web.Class
import           Gen.AST.Types              (Data (..), ExternalTypeName (..),
                                             Field (..), TypeName (..))
import qualified Gen.AST.Types              as G
import           Gen.AST.Web.Get
import           Gen.AST.Web.Path           (PathSegment (..))
import qualified Gen.AST.Web.Path           as P

data GetInfo = GetInfo
  { _giGet  :: Get
  , _giPath :: [P.PathSegment]
  , _giData :: Data
  } deriving (Show, Eq)

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

getRequestType
  :: (MonadWebCode m)
  => Get -> m Data
getRequestType Get {..} = do
  name <- requestName _getPathTemplate
  return
    Data
    { _dataName = name
    , _dataFields = queryFields _getQueryParams <> pathFields _getPathParams
    , _dataAddlFields = Nothing
    , _dataDescription = _getDescription
    }

requestName :: (MonadWebCode m)
            => Text -- ^ path template
            -> m ExternalTypeName
requestName pathText = do
  pathTypes <- _wcePathTypes <$> ask
  path <- case f pathText of
    Left err  -> throwWebCodeError pathText err
    Right val -> return val
  return $ pathTypes path
  where f = runParser P.parsePath () ""

queryFields :: Params QueryParam -> [Field]
queryFields = paramFields queryParamField

queryFieldType :: QueryParamType -> TypeName
queryFieldType QueryParamBool   = boolName
queryFieldType QueryParamInt    = int64Name
queryFieldType QueryParamString = textName

pathFields :: Params PathParam -> [Field]
pathFields = paramFields pathParamField

queryParamFieldPrefix = "query"
pathParamFieldPrefix = "path"

mkParamFieldName :: Text -- ^ Prefix for param type
                 -> Text -- ^ Param name
                 -> Text -- ^ Field name
mkParamFieldName prefix = mappend prefix . toTitle

queryParamField :: Text -> QueryParam -> Field
queryParamField paramName QueryParam {..} =
  Field
  { _fieldName = mkParamFieldName queryParamFieldPrefix paramName
  , _fieldType = queryFieldType _paramType
  , _fieldDescription = _paramDescription
  , _fieldRequired = _paramRequired
  }

pathParamField :: Text -> PathParam -> Field
pathParamField paramName PathParam {..} =
  Field
  { _fieldName = mkParamFieldName pathParamFieldPrefix paramName
  , _fieldType = textName
  , _fieldDescription = _paramDescription
  , _fieldRequired = True
  }

paramFields :: (Text -> param -> Field) -> Params param -> [Field]
paramFields paramToField = fmap (uncurry paramToField) . HI.toList
