{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Gen.AST.Error where

import           Control.Monad.Except
import Control.Lens

import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as T

data ASTError = ASTError
  { _asteStack :: [Text]
  , _asteError :: Text
  } deriving (Show)

makeLenses ''ASTError

type ASTExcept = Except ASTError

mkASTError :: (Show a) => Text -> a -> ASTError
mkASTError message context = ASTError [] $ message <> T.pack (show context)

throwASTError :: (MonadError ASTError m, Show a) => Text -> a -> m b
throwASTError message context = throwError $ mkASTError message context

pushASTError :: (MonadError ASTError m, Show b) => b -> m a -> m a
pushASTError context action = catchError action (throwError . f)
  where f err = err & asteStack %~ (T.pack (show context):)
