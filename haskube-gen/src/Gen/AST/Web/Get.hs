{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Gen.AST.Web.Get where

import qualified Data.HashMap.Strict.InsOrd as HI
import           Data.Text                  (Text, unpack)

import qualified Gen.AST.BuiltIn            as G
import           Gen.AST.Code.Combinators
import           Gen.AST.Code.Data
import           Gen.AST.Code.Types
import qualified Gen.AST.Types              as G


-- | Parameters by name.
type Params v = HI.InsOrdHashMap Text v

data QueryParamType
  = QueryParamBool
  | QueryParamInt
  | QueryParamString
  deriving (Show, Eq)

data QueryParam = QueryParam
  { _paramType        :: QueryParamType
  , _paramDescription :: Maybe Text
  , _paramRequired    :: Bool
  } deriving (Show, Eq)

data PathParam = PathParam
  { _paramDescription :: Maybe Text
  } deriving (Show, Eq)

-- TODO: Other kinds of params? (Header, FormData, Body)
-- | All path params are strings! (TODO: verify?)

-- TODO(low priority): Safe to ignore consumes/produces?
-- TODO: Special case for "watch" param. (Streaming JSON)
data Get = Get
  { _getPathTemplate :: Text
  , _getQueryParams  :: Params QueryParam
  , _getPathParams   :: Params PathParam
  , _getDescription  :: Maybe Text
  , _getResponseType :: G.TypeName -- ^ 200 should come with data (TODO: verify)
  } deriving (Show, Eq)
