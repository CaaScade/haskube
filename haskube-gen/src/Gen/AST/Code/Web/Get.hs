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
import           Text.Show.Pretty           (pPrint)

import           Gen.AST.BuiltIn
import           Gen.AST.Code.Combinators
import           Gen.AST.Code.Types
import           Gen.AST.Code.Web.Class
import           Gen.AST.Types              (Data (..), ExternalTypeName (..),
                                             Field (..), TypeName (..))
import qualified Gen.AST.Types              as G
import           Gen.AST.Web.Get
import qualified Gen.AST.Web.Path           as P

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
queryFields = paramFields "query" queryFieldType

queryFieldType :: QueryParamType -> TypeName
queryFieldType QueryParamBool   = boolName
queryFieldType QueryParamInt    = int64Name
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

testGet :: Get
testGet =
  Get
  { _getPathTemplate = "/api/doot"
  , _getQueryParams = HI.empty
  , _getPathParams = HI.empty
  , _getDescription = Just "I am doot."
  , _getResponseType = G.SimpleName (Just "Api.Base") "Doot"
  }

testEnv :: WebCodeEnv
testEnv = WebCodeEnv {_wcePathTypes = f}
  where
    f _ = G.ExternalName "Client.Api" "GetDoot"

test :: IO ()
test = do
  result <- runExceptT $ flip runReaderT testEnv $ getRequestType testGet
  pPrint result
