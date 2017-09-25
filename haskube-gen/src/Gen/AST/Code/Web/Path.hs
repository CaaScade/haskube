{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gen.AST.Code.Web.Path where

import           Language.Haskell.Exts

import           Control.Monad

import Data.List (intersperse)
import           Data.Foldable            (foldl')
import           Data.Maybe               (isNothing, maybeToList)
import           Data.Monoid
import           Data.Text                (Text, unpack)

import qualified Gen.AST.BuiltIn          as G
import           Gen.AST.Code.Combinators
import           Gen.AST.Code.Common
import           Gen.AST.Code.Data
import           Gen.AST.Code.Types
import           Gen.AST.Code.Web.Class
import           Gen.AST.Code.Web.Get
import qualified Gen.AST.Types            as G
import           Gen.AST.Web.Get
import           Gen.AST.Web.Path

nRequestPath :: Name Ann
nRequestPath = mkIdent "requestPath"

-- | Variable for a field (like it's been record-wildcarded)
mkPathParamName :: Text -- ^ param name without prefix or leading underscore
                -> Exp Ann
mkPathParamName = mkFieldName . mkParamFieldName pathParamFieldPrefix

mkRequestPathRHS_ :: [PathSegment] -> Exp Ann
mkRequestPathRHS_ segments = foldl' (\x y -> mkInfixApp x xDiamond y) slash exps
  where
    slash = mkString "/"
    exps = intersperse slash (f <$> segments)
    f (ConstSegment text) = mkString text
    f (ParamSegment name) = mkPathParamName name

mkRequestPathRHS
  :: [PathSegment] -> Rhs Ann
mkRequestPathRHS = UnGuardedRhs mempty . mkRequestPathRHS_

mkRequestPath :: GetInfo -> InstDecl Ann
mkRequestPath GetInfo {..} = InsDecl mempty $ FunBind mempty [match]
  where
    conName = G._externalName _dataName
    Get {..} = _giGet
    G.Data {..} = _giData
    pat =
      if null _dataFields && isNothing _dataAddlFields
        then pCon conName
        else pRecordWildcard conName
    rhs = mkRequestPathRHS _giPath
    match = Match mempty nRequestPath [pat] rhs Nothing
