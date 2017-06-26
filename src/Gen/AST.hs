{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Gen.AST where

import           Control.Applicative
import           Control.Monad

import           Control.Lens

import qualified Data.HashMap.Strict.InsOrd as HI
import           Data.Monoid
import qualified Data.Swagger               as S
import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           Gen.AST.Error
import           Gen.AST.Name               (TypeName (..), schemaTypeName)

rewriteDefinitions :: S.Definitions S.Schema -> ASTExcept [Type]
rewriteDefinitions definitions = rewriteDefinition <$> HI.toList definitions

-- | Figure out if this definition needs a newtype or a data declaration.
-- TODO: This might be easier if I don't treat TypeName and Type as separate flows.
--       I probably should account for nested object types and accumulate Types as I traverse the tree.
rewriteDefinition :: (Text, S.Schema) -> ASTExcept Type
rewriteDefinition (key, schema) =
  case S._paramSchemaType . S._schemaParamSchema $ schema of
    S.SwaggerString  -> Left $ mkNewtype schema stringTypeName
    S.SwaggerNumber  -> Left $ mkNewtype schema numberTypeName
    S.SwaggerInteger -> Left $ mkNewtype schema integerTypeName
    S.SwaggerBoolean -> Left $ mkNewtype schema boolTypeName
    S.SwaggerArray   -> _
    S.SwaggerObject  -> _
  where
    typeName = keyedTypeName key
