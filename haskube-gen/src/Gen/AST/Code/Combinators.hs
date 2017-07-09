{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gen.AST.Code.Combinators where

import           Control.Monad.Reader

import           Language.Haskell.Exts

import           Data.Monoid
import           Data.Text             (Text, unpack)

import           Gen.AST.Code.Types
import qualified Gen.AST.Types         as G

mkModuleName :: Text -> ModuleName Ann
mkModuleName = ModuleName mempty . unpack

mkIdent :: Text -> Name Ann
mkIdent = Ident mempty . unpack

mkSymbol :: Text -> Name Ann
mkSymbol = Symbol mempty . unpack

mkUnqual :: Text -> QName Ann
mkUnqual = UnQual mempty . mkIdent

-- | The underscore means it's for symbols. (lol)
mkUnqual_ :: Text -> QName Ann
mkUnqual_ = UnQual mempty . mkSymbol

mkQual :: Text -> Text -> QName Ann
mkQual moduleName = Qual mempty (mkModuleName moduleName) . mkIdent

mkQual_ :: Text -> Text -> QName Ann
mkQual_ moduleName = Qual mempty (mkModuleName moduleName) . mkSymbol

mkQVarOp :: Text -> Text -> QOp Ann
mkQVarOp moduleName = QVarOp mempty . mkQual moduleName

mkQVarOp_ :: Text -> Text -> QOp Ann
mkQVarOp_ moduleName = QVarOp mempty . mkQual_ moduleName

mkQVarOp' :: Text -> QOp Ann
mkQVarOp' = QVarOp mempty . mkUnqual

mkQVarOp_' :: Text -> QOp Ann
mkQVarOp_' = QVarOp mempty . mkUnqual_

mkVarExp :: Text -> Text -> Exp Ann
mkVarExp moduleName = Var mempty . mkQual moduleName

mkVarExp' :: Text -> Exp Ann
mkVarExp' = Var mempty . mkUnqual

mkQName :: (MonadModule m) => Maybe Text -> Text -> m (QName Ann)
mkQName Nothing typeName = return . mkUnqual $ typeName
mkQName (Just moduleName) typeName = do
  currentModule <- ask
  return $
    if currentModule == moduleName
      then mkUnqual typeName
      else mkQual moduleName typeName

mkTyApp :: [Type Ann] -> Type Ann
mkTyApp types = foldl1 f types
  where f accum0 aType = TyApp mempty accum0 aType

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
mkType (G.MaybeName valueName) = do
  maybeType <- mkType' Nothing "Maybe"
  valueType <- mkType valueName
  return $ mkTyApp [maybeType, valueType]
mkType (G.SimpleName moduleName typeName) = mkType' moduleName typeName

mkApp :: Exp Ann -> Exp Ann -> Exp Ann
mkApp = App mempty

mkInfixApp :: Exp Ann -> QOp Ann -> Exp Ann -> Exp Ann
mkInfixApp = InfixApp mempty

-- | Make a string literal.
mkString :: Text -> Exp Ann
mkString text_ = Lit mempty $ String mempty text text
  where text = unpack text_

mkInstHead :: QName Ann -> Type Ann -> InstHead Ann
mkInstHead className aType =
  IHApp mempty (IHCon mempty className) aType

mkInstRule :: QName Ann -> Type Ann -> InstRule Ann
mkInstRule className aType =
  IRule mempty Nothing Nothing $ mkInstHead className aType

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
