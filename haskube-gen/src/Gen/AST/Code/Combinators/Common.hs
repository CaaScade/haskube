{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Gen.AST.Code.Combinators.Common where

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

mkApp :: Exp Ann -> Exp Ann -> Exp Ann
mkApp = App mempty

mkInfixApp :: Exp Ann -> QOp Ann -> Exp Ann -> Exp Ann
mkInfixApp = InfixApp mempty

-- | Make a string literal.
mkString :: Text -> Exp Ann
mkString text_ = Lit mempty $ String mempty text text
  where text = unpack text_
