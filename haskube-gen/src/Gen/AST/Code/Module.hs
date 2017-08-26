{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gen.AST.Code.Module where

import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Either              (either)
import           Data.List                (nub)
import           Data.Monoid
import           Data.Text                (Text)

import           Language.Haskell.Exts

import qualified Data.HashMap.Strict      as H

import qualified Gen.AST.BuiltIn          as G
import           Gen.AST.Code.Combinators
import           Gen.AST.Code.Data
import           Gen.AST.Code.JSON
import           Gen.AST.Code.Types
import qualified Gen.AST.Types            as G
import           Gen.AST.Unflatten        (importedModules)
import qualified Gen.AST.Unflatten        as G

mkImport :: (MonadModule m) => Text -> m (ImportDecl Ann)
mkImport moduleName =
  return
    ImportDecl
    { importAnn = mempty
    , importModule = mkModuleName moduleName
    , importQualified = True
    , importSrc = False
    , importSafe = False
    , importPkg = Nothing
    , importAs = Nothing
    , importSpecs = Nothing
    }

builtInImport :: ImportDecl Ann
builtInImport =
  ImportDecl
  { importAnn = mempty
  , importModule = mkModuleName G.builtInNewtypesModule'
  , importQualified = True
  , importSrc = False
  , importSafe = False
  , importPkg = Nothing
  , importAs = Nothing
  , importSpecs = Nothing
  }

defaultImports :: [ImportDecl Ann]
defaultImports =  maybeImport : builtInImport : hashMapImport : aesonImports

mkImports :: (MonadModule m) => [Text] -> m [ImportDecl Ann]
mkImports modules = do
  currentModule <- ask
  mapM mkImport $ filter (/= currentModule) modules

mkModule :: Text -> [G.Type] -> Module Ann
mkModule moduleName types =
  Module mempty (Just moduleHead) pragmas imports (dataDecls <> instDecls)
  where
    run = flip runReader moduleName
    imports = nub $ defaultImports <> (run . mkImports $ importedModules types)
    dataDecls = run . mapM (either mkNewtype mkData) $ types
    instDecls = mkJSONs types
    pragmas =
      [ mkLanguagePragma "DuplicateRecordFields"
      , mkLanguagePragma "RecordWildCards"
      , mkLanguagePragma "OverloadedStrings"
      ]
    moduleHead = ModuleHead mempty (mkModuleName moduleName) Nothing Nothing

mkModules :: [G.Type] -> H.HashMap Text (Module Ann)
mkModules = H.mapWithKey mkModule . G.toModules
