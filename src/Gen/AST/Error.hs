{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}

module Gen.AST.Error where

import           Control.Monad.Except
import           Control.Lens
import           Control.Monad.Writer

import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Text.PrettyPrint     (Doc)
import           Text.Show.Pretty     (ppDoc)

import Gen.AST.Types

data ASTError = ASTError
  { _asteStack :: [Doc]
  , _asteLabel :: Text
  , _asteError :: Doc
  } deriving (Show)

makeLenses ''ASTError

type MonadASTError m = MonadError ASTError m
type MonadASTWriter m = MonadWriter ASTLog m
type MonadAST m = (MonadASTError m, MonadASTWriter m)

type ASTLog = [Type]
type ASTExcept = Except ASTError
type ASTBuildT m = ExceptT ASTError (WriterT ASTLog m)
type ASTBuild = ASTBuildT Identity

mkASTError :: (Show a) => Text -> a -> ASTError
mkASTError message context = ASTError [] message $ ppDoc context

throwASTError :: (MonadError ASTError m, Show a) => Text -> a -> m b
throwASTError message context = throwError $ mkASTError message context

pushASTError :: (MonadError ASTError m, Show b) => b -> m a -> m a
pushASTError context action = catchError action (throwError . f)
  where f err = err & asteStack %~ (ppDoc context:)

tellNewtype :: (MonadASTWriter m) => Newtype -> m ()
tellNewtype = tell . pure . Left

tellData :: (MonadASTWriter m) => Data -> m ()
tellData = tell . pure. Right
