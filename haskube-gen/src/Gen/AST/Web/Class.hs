{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Gen.AST.Web.Class where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Writer

import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Text.PrettyPrint     (Doc)
import           Text.Show.Pretty     (ppDoc)

import           Gen.AST.Class
import           Gen.AST.Types
import           Gen.AST.Web.Path

newtype ASTWebEnv = ASTWebEnv
  { _astwPathTypes :: [PathSegment] -> ExternalTypeName }

type MonadASTWebReader m = MonadReader ASTWebEnv m
type MonadASTWeb m = (MonadASTError m, MonadASTWebReader m)
