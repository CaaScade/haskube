{-# LANGUAGE OverloadedStrings #-}

module Gen.AST.Code.Common where

import           Language.Haskell.Exts

import           Gen.AST.Code.Combinators
import           Gen.AST.Code.Types

monoidImport :: ImportDecl Ann
monoidImport =
  ImportDecl
  { importAnn = mempty
  , importModule = mkModuleName "Data.Monoid"
  , importQualified = False
  , importSrc = False
  , importSafe = False
  , importPkg = Nothing
  , importAs = Nothing
  , importSpecs = Nothing
  }

xDiamond :: QOp Ann
xDiamond = mkQVarOp_' "<>"

xDot :: QOp Ann
xDot = mkQVarOp_' "."

xFmap :: Exp Ann
xFmap = mkVarExp' "fmap"
