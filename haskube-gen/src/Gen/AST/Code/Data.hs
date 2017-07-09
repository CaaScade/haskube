{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gen.AST.Code.Data where

import           Control.Monad.Reader

import           Language.Haskell.Exts

import           Data.Maybe               (maybeToList)
import           Data.Text                (Text)

import           Gen.AST.Code.Combinators
import           Gen.AST.Code.Types
import qualified Gen.AST.Types            as G

mkLanguagePragma :: Text -> ModulePragma Ann
mkLanguagePragma = LanguagePragma mempty . pure . mkIdent

mkFieldDecl :: Text -> Type Ann -> FieldDecl Ann
mkFieldDecl name aType = FieldDecl mempty [mkIdent name] aType

-- | NOTE: Prefixes the field name with "_".
mkFieldDecl' :: (MonadModule m) => G.Field -> m (FieldDecl Ann)
mkFieldDecl' G.Field {..} = do
  let name = either (const "additionalProperties") id $ _fieldName
      description = maybeToList _fieldDescription
  fieldType <- mkType _fieldType
  return $ FieldDecl description [mkIdent $ toRecordFieldName name] fieldType

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
  return . mkQualConDecl $ mkRecDecl' name (toNewtypeFieldName name) aType

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
