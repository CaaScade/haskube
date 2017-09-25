{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Gen.AST.Code.Combinators.Class where

import           Language.Haskell.Exts

import           Data.Monoid
import           Data.Text             (Text, unpack)

import           Gen.AST.Code.Types
import qualified Gen.AST.Types         as G

mkInstHead :: QName Ann -> Type Ann -> InstHead Ann
mkInstHead className = IHApp mempty (IHCon mempty className)

mkInstRule :: QName Ann -> Type Ann -> InstRule Ann
mkInstRule className aType =
  IRule mempty Nothing Nothing $ mkInstHead className aType
