{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gen.AST.Code where

import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Maybe            (maybeToList)
import           Data.Monoid
import           Data.Text             (Text, pack, unpack)
import Data.Either (either)

import           Language.Haskell.Exts

import           Data.HashMap.Strict   as H

import qualified Gen.AST.Types         as G

type Ann = [Text]

type MonadModule m = MonadReader Text m
type ModuleT m = ReaderT Text m

mkIdent :: Text -> Name Ann
mkIdent = Ident mempty . unpack

mkUnqual :: Text -> QName Ann
mkUnqual = UnQual mempty . mkIdent

mkQual :: Text -> Text -> QName Ann
mkQual moduleName = Qual mempty (ModuleName mempty $ unpack moduleName) . mkIdent

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
  stringType <- mkType' Nothing "String"
  valueType <- mkType valueName
  return $ mkTyApp [dictType, stringType, valueType]
mkType (G.MaybeName valueName) = do
  maybeType <- mkType' Nothing "Maybe"
  valueType <- mkType valueName
  return $ mkTyApp [maybeType, valueType]
mkType (G.SimpleName moduleName typeName) = mkType' moduleName typeName

mkFieldDecl :: Text -> Type Ann -> FieldDecl Ann
mkFieldDecl name aType = FieldDecl mempty [mkIdent name] aType

mkFieldDecl' :: (MonadModule m) => G.Field -> m (FieldDecl Ann)
mkFieldDecl' G.Field {..} = do
  let name = either (const "additionalModules") id $ _fieldName
      description = maybeToList _fieldDescription
  fieldType <- mkType _fieldType
  return $ FieldDecl description [mkIdent name] fieldType

mkConDecl' :: Text -> Type Ann -> ConDecl Ann
mkConDecl' name = ConDecl mempty (mkIdent name) . pure

mkRecDecl :: Text -> [FieldDecl Ann] -> ConDecl Ann
mkRecDecl conName = RecDecl mempty (mkIdent conName)

mkRecDecl' :: Text -> Text -> Type Ann -> ConDecl Ann
mkRecDecl' conName fieldName fieldType =
  RecDecl mempty (mkIdent conName) [mkFieldDecl fieldName fieldType]

mkQualConDecl :: ConDecl Ann -> QualConDecl Ann
mkQualConDecl = QualConDecl mempty Nothing Nothing

mkNewtypeRHS :: (MonadModule m) => Text -> G.TypeName -> m (QualConDecl Ann)
mkNewtypeRHS name typeName = do
  aType <- mkType typeName
  return . mkQualConDecl $ mkRecDecl' name ("un" <> name) aType

mkDataRHS :: (MonadModule m) => Text -> [G.Field] -> m (QualConDecl Ann)
mkDataRHS name fields_ = do
  fields <- mapM mkFieldDecl' fields_
  return . mkQualConDecl $ mkRecDecl name fields

mkDataLHS :: Text -> DeclHead Ann
mkDataLHS = DHead mempty . mkIdent

-- | NOTE: Assumes this declaration belongs in the current module.
mkNewtype :: (MonadModule m) => G.Newtype -> m (Decl Ann)
mkNewtype G.Newtype{..} = do
  let name = G._externalName _newtypeName
      lhs = mkDataLHS name
      comment = maybeToList _newtypeDescription
  rhs <- mkNewtypeRHS name _newtypeValue
  return $ DataDecl comment (NewType mempty) Nothing lhs [rhs] Nothing

-- | NOTE: Assumes this declaration belongs in the current module.
mkData :: (MonadModule m) => G.Data -> m (Decl Ann)
mkData G.Data{..} = do
  let name = G._externalName _dataName
      lhs = mkDataLHS name
      comment = maybeToList _dataDescription
  rhs <- mkDataRHS name _dataFields
  return $ DataDecl comment (DataType mempty) Nothing lhs [rhs] Nothing
