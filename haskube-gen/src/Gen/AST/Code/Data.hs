{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gen.AST.Code.Data where

import           Control.Monad.Reader

import           Language.Haskell.Exts

import           Data.Maybe               (maybeToList)
import           Data.Monoid
import           Data.Text                (Text)

import           Gen.AST.Code.Combinators
import           Gen.AST.Code.Types
import qualified Gen.AST.Types            as G

mkFieldDecl :: Text -> Type Ann -> FieldDecl Ann
mkFieldDecl name = FieldDecl mempty [mkIdent name]

addlFieldsFieldName :: Text
addlFieldsFieldName = "additionalProperties"

-- | NOTE: Prefixes the field name with "_".
mkAddlFieldsDecl :: (MonadModule m) => G.AddlFields -> m (FieldDecl Ann)
mkAddlFieldsDecl G.AddlFields {..} = do
  let description = maybeToList _addlFieldsDescription
  fieldType <- mkType $ G.DictionaryName _addlFieldsType
  return $ FieldDecl description [mkIdent $ toRecordFieldName addlFieldsFieldName] fieldType

-- | NOTE: Prefixes the field name with "_".
mkFieldDecl' :: (MonadModule m) => G.Field -> m (FieldDecl Ann)
mkFieldDecl' G.Field {..} = do
  let description = maybeToList _fieldDescription
  fieldType <- mkFieldType _fieldType _fieldRequired
  return $ FieldDecl description [mkIdent $ toRecordFieldName _fieldName] fieldType

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

mkDataRHS :: (MonadModule m) => Text -> [G.Field] -> Maybe G.AddlFields -> m (QualConDecl Ann)
mkDataRHS name fields_ addlFields_ = do
  fields <- mapM mkFieldDecl' fields_
  addlFields <- traverse mkAddlFieldsDecl addlFields_
  return . mkQualConDecl $ mkRecDecl name (fields <> maybeToList addlFields)

mkDataLHS :: Text -> DeclHead Ann
mkDataLHS = DHead mempty . mkIdent

-- | NOTE: Assumes this declaration belongs in the current module.
mkNewtype :: (MonadModule m) => G.Newtype -> m (Decl Ann)
mkNewtype G.Newtype{..} = do
  let name = G._externalName _newtypeName
      lhs = mkDataLHS name
      comment = maybeToList _newtypeDescription
  rhs <- mkNewtypeRHS name _newtypeValue
  return $ DataDecl comment (NewType mempty) Nothing lhs [rhs] $ Just derivings

-- | NOTE: Assumes this declaration belongs in the current module.
mkData :: (MonadModule m) => G.Data -> m (Decl Ann)
mkData G.Data{..} = do
  let name = G._externalName _dataName
      lhs = mkDataLHS name
      comment = maybeToList _dataDescription
  rhs <- mkDataRHS name _dataFields _dataAddlFields
  return $ DataDecl comment (DataType mempty) Nothing lhs [rhs] $ Just derivings

derivings :: Deriving Ann
derivings = Deriving mempty [mkDeriving "Show", mkDeriving "Eq"]

mkDeriving :: Text -> InstRule Ann
mkDeriving = IRule mempty Nothing Nothing . IHCon mempty . mkUnqual
