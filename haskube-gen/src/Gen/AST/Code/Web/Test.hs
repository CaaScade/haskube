{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Gen.AST.Code.Web.Test where

import           Language.Haskell.Exts.Pretty (prettyPrint)

import           Control.Monad.Except
import           Control.Monad.Reader

import qualified Data.HashMap.Strict.InsOrd   as HI
import           Text.Show.Pretty             (pPrint)

import           Gen.AST.Code.Web.Class
import           Gen.AST.Code.Web.Get
import           Gen.AST.Code.Web.Path
import           Gen.AST.Types                (Data (..), ExternalTypeName (..),
                                               Field (..), TypeName (..))
import qualified Gen.AST.Types                as G
import           Gen.AST.Web.Get
import           Gen.AST.Web.Path             (PathSegment (..))

testGet :: Get
testGet =
  Get
  { _getPathTemplate = "/api/doot/{id}"
  , _getQueryParams = HI.fromList qps
  , _getPathParams = HI.fromList pps
  , _getDescription = Just "I am doot."
  , _getResponseType = G.SimpleName (Just "Api.Base") "Doot"
  }
  where qps = [("bloo", QueryParam QueryParamBool Nothing False)]
        pps = [("id", PathParam Nothing)]

testEnv :: WebCodeEnv
testEnv = WebCodeEnv {_wcePathTypes = f}
  where
    f _ = G.ExternalName "Client.Api" "GetDoot"

testData :: Data
testData =
  Data
  { _dataName =
      ExternalName {_externalModule = "Client.Api", _externalName = "GetDoot"}
  , _dataFields =
      [ Field
        { _fieldName = "queryBloo"
        , _fieldType = SimpleName Nothing "Bool"
        , _fieldDescription = Nothing
        , _fieldRequired = False
        }
      , Field
        { _fieldName = "pathId"
        , _fieldType = SimpleName (Just "Data.Text") "Text"
        , _fieldDescription = Nothing
        , _fieldRequired = True
        }
      ]
  , _dataAddlFields = Nothing
  , _dataDescription = Just "I am doot."
  }

testPath :: [PathSegment]
testPath = [ConstSegment "api", ConstSegment "doot", ParamSegment "id"]

testInfo :: GetInfo
testInfo = GetInfo testGet testPath testData

testRequestType :: IO ()
testRequestType = do
  result <- runExceptT $ flip runReaderT testEnv $ getRequestType testGet
  pPrint result

testRequestPath :: IO ()
testRequestPath = putStrLn $ prettyPrint $ mkRequestPath testInfo
