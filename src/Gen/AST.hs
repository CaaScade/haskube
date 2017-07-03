{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Gen.AST where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Writer

import           Control.Lens

import           Data.Foldable              (foldl')
import qualified Data.HashMap.Strict.InsOrd as HI
import qualified Data.HashMap.Strict        as H
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid
import qualified Data.Swagger               as S
import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           Gen.AST.Class
import           Gen.AST.Name               (keyedTypeName, schemaTypeName)
import           Gen.AST.Types

type Modules = H.HashMap Text [Type]

rewriteDefinitions :: S.Definitions S.Schema -> Either ASTError [Type]
rewriteDefinitions = runExcept . execWriterT . rewriteDefinitions_

rewriteDefinitions_ :: (MonadAST m) => S.Definitions S.Schema -> m ()
rewriteDefinitions_ definitions =
  pushASTError "rewriteDefinitions_" $
  mapM_ rewriteDefinition $ HI.toList definitions

rewriteDefinition :: (MonadAST m) => (Text, S.Schema) -> m TypeName
rewriteDefinition (key, schema) =
  pushASTError ("rewriteDefinition", key) $ do
    typeName <- keyedTypeName key
    fst <$> schemaTypeName (Required typeName) schema

-- | Group definitions by module
toModules :: [Type] -> Modules
toModules types = reverse <$> foldl' f H.empty types
  where
    f :: Modules -> Type -> Modules
    f modules0 aType = H.insert aModule (aType:moduleTypes) modules0
      where
        aModule = _typeModule aType
        moduleTypes = fromMaybe [] $ H.lookup aModule modules0
