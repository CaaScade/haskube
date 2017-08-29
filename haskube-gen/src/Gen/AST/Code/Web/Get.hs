{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Gen.AST.Code.Web.Get where

import           Language.Haskell.Exts

import qualified Data.HashMap.Strict.InsOrd as HI
import           Data.Text                  (Text, unpack)

import qualified Gen.AST.BuiltIn            as G
import           Gen.AST.Code.Combinators
import           Gen.AST.Code.Data
import           Gen.AST.Code.Types
import qualified Gen.AST.Types              as G

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

-- | Parameters by name.
type Params v = HI.InsOrdHashMap Text v

data Param t = Param
  { _paramType        :: t
  , _paramDescription :: Maybe Text
  , _paramRequired    :: Bool
  } deriving (Show)

data QueryParamType
  = QueryParamBool
  | QueryParamInt
  | QueryParamString
  deriving (Show)

-- TODO: Other kinds of params? (Header, FormData)
-- | All path params are strings! (TODO: verify?)
type PathParam = Param ()
type QueryParam = Param QueryParamType
type BodyParam = Param G.TypeName

-- TODO(low priority): Safe to ignore consumes/produces?
-- TODO: Special case for "watch" param. (Streaming JSON)
data Get = Get
  { _getQueryParams  :: Params QueryParam
  , _getPathParams   :: Params PathParam
  , _getBodyParam    :: BodyParam
  , _getDescription  :: Maybe Text
  , _getResponseType :: G.TypeName -- ^ 200 should come with data (TODO: verify)
  } deriving (Show)
