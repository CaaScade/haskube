{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gen.AST.Code.Combinators.Type where

import           Language.Haskell.Exts

import           Data.Monoid
import           Data.Text                       (Text)

import           Gen.AST.Code.Combinators.Common
import           Gen.AST.Code.Types
import qualified Gen.AST.Types                   as G

mkTyApp :: [Type Ann] -> Type Ann
mkTyApp = foldl1 (TyApp mempty)

mkFieldType :: (MonadModule m) => G.TypeName -> Bool -> m (Type Ann)
mkFieldType typeName True = mkType typeName
mkFieldType typeName False = do
  maybeType <- mkType' Nothing "Maybe"
  valueType <- mkType typeName
  return $ mkTyApp [maybeType, valueType]

mkType' :: (MonadModule m) => Maybe Text -> Text -> m (Type Ann)
mkType' moduleName typeName = TyCon mempty <$> mkQName moduleName typeName

mkType :: (MonadModule m) => G.TypeName -> m (Type Ann)
mkType (G.ArrayName valueName) = do
  valueType <- mkType valueName
  return $ TyList mempty valueType
mkType (G.TupleName valueNames) = do
  valueTypes <- mapM mkType valueNames
  return $ TyTuple mempty Boxed valueTypes
mkType (G.DictionaryName valueName) = do
  dictType <- mkType' (Just "Data.HashMap.Strict") "HashMap"
  stringType <- mkType' (Just "Data.Text") "Text"
  valueType <- mkType valueName
  return $ mkTyApp [dictType, stringType, valueType]
mkType (G.SimpleName moduleName typeName) = mkType' moduleName typeName

tNewtype :: G.Newtype -> Type Ann
tNewtype G.Newtype{..} = TyCon mempty . mkUnqual $ G._externalName _newtypeName

tData :: G.Data -> Type Ann
tData G.Data{..} = TyCon mempty . mkUnqual $ G._externalName _dataName

-- | Turn a newtype constructor name into the field name.
toNewtypeFieldName :: Text -> Text
toNewtypeFieldName conName = "_un" <> conName

toRecordFieldName :: Text -> Text
toRecordFieldName fieldName = "_" <> fieldName

-- | Variable for a field (like it's been record-wildcarded)
mkFieldName :: Text -- ^ field name without the leading underscore
            -> Exp Ann
mkFieldName = mkVarExp' . toRecordFieldName

pRecordWildcard :: Text -> Pat Ann
pRecordWildcard conName = PRec mempty (mkUnqual conName) [PFieldWildcard mempty]

pCon :: Text -> Pat Ann
pCon conName = PApp mempty (mkUnqual conName) []
