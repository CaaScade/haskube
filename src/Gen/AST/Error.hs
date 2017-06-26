{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}

module Gen.AST.Error where

import           Control.Monad.Except
import Control.Lens
import Control.Monad.Writer

import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as T

import Gen.AST.Types

data ASTError = ASTError
  { _asteStack :: [Text]
  , _asteError :: Text
  } deriving (Show)

makeLenses ''ASTError

type MonadASTError m = MonadError ASTError m

type ASTExcept = Except ASTError
type ASTBuildT m = ExceptT ASTError (WriterT [Type] m)
type ASTBuild = ASTBuildT Identity

mkASTError :: (Show a) => Text -> a -> ASTError
mkASTError message context = ASTError [] $ message <> T.pack (show context)

throwASTError :: (MonadError ASTError m, Show a) => Text -> a -> m b
throwASTError message context = throwError $ mkASTError message context

pushASTError :: (MonadError ASTError m, Show b) => b -> m a -> m a
pushASTError context action = catchError action (throwError . f)
  where f err = err & asteStack %~ (T.pack (show context):)
