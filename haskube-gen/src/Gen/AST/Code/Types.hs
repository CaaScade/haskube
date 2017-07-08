{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Gen.AST.Code.Types where

import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Text            (Text)

type Ann = [Text]

type MonadModule m = MonadReader Text m
type ModuleT m = ReaderT Text m
