{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gen.AST.Code.Combinators
  ( module Gen.AST.Code.Combinators
  , module Gen.AST.Code.Combinators.Class
  , module Gen.AST.Code.Combinators.Common
  , module Gen.AST.Code.Combinators.Module
  , module Gen.AST.Code.Combinators.Type
  ) where

import           Language.Haskell.Exts

import           Data.Monoid
import           Data.Text                       (Text, unpack)

import           Gen.AST.Code.Combinators.Class
import           Gen.AST.Code.Combinators.Common
import           Gen.AST.Code.Combinators.Module
import           Gen.AST.Code.Combinators.Type
import           Gen.AST.Code.Types
import qualified Gen.AST.Types                   as G

xAddlProps :: Exp Ann
xAddlProps = mkVarExp' "_additionalProperties"
