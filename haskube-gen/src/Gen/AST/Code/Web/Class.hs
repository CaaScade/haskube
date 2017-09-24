{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Gen.AST.Code.Web.Class where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Writer

import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Text.PrettyPrint     (Doc)
import           Text.Show.Pretty     (ppDoc)

import           qualified Gen.AST.Types as G
import           qualified Gen.AST.Web.Path as G

data WebCodeEnv = WebCodeEnv
  { _wcePathTypes :: [G.PathSegment] -> G.ExternalTypeName }

data WebCodeError = WebCodeError
  { _wceLabel :: Text
  , _wceError :: Doc } deriving (Show, Eq)

type MonadWebCodeError m = MonadError WebCodeError m
type MonadWebCodeReader m = MonadReader WebCodeEnv m
type MonadWebCode m = (MonadWebCodeError m, MonadWebCodeReader m)

mkWebCodeError :: (Show a) => Text -> a -> WebCodeError
mkWebCodeError message context = WebCodeError message $ ppDoc context

throwWebCodeError :: (MonadWebCodeError m, Show a) => Text -> a -> m b
throwWebCodeError message context = throwError $ mkWebCodeError message context
