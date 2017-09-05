{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Gen.AST.Code.Web.Get where

import           Data.Text                (Text)
import           Language.Haskell.Exts

import           Gen.AST.Code.Combinators
import           Gen.AST.Code.Types
import qualified Gen.AST.Web.Get as G

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


