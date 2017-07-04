{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Gen.AST.Unflatten where

import           Control.Monad.Reader
import           Control.Monad.State

import           Data.Either          (either)
import           Data.Foldable        (foldl')
import qualified Data.HashMap.Strict  as H
import qualified Data.HashSet         as HS
import           Data.List            (sort)
import           Data.Maybe           (fromMaybe)
import           Data.Monoid
import           Data.Text            (Text)

import           Gen.AST.Types

type ImportedModules = HS.HashSet Text
type Modules = H.HashMap Text [Type]

importedModules :: [Type] -> [Text]
importedModules = sort . HS.toList . importedModules_

importedModules_ :: [Type] -> ImportedModules
importedModules_ = foldl' (<>) HS.empty . fmap typeImportedModules

typeImportedModules :: Type -> ImportedModules
typeImportedModules = either newtypeImportedModules dataImportedModules

dataImportedModules :: Data -> ImportedModules
dataImportedModules Data{..} =
  foldl' (<>) HS.empty $ fieldImportedModules <$> _dataFields

newtypeImportedModules :: Newtype -> ImportedModules
newtypeImportedModules Newtype{..} =
  HS.fromList $ _typeNameModules _newtypeValue

fieldImportedModules :: Field -> ImportedModules
fieldImportedModules Field{..} =
  HS.fromList $ _typeNameModules _fieldType

-- | Group definitions by module
toModules :: [Type] -> Modules
toModules types = reverse <$> foldl' f H.empty types
  where
    f :: Modules -> Type -> Modules
    f modules0 aType = H.insert aModule (aType:moduleTypes) modules0
      where
        aModule = _typeModule aType
        moduleTypes = fromMaybe [] $ H.lookup aModule modules0
