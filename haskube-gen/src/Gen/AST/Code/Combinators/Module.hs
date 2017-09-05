module Gen.AST.Code.Combinators.Module where

import Data.Text (Text)

import Language.Haskell.Exts

import Gen.AST.Code.Types
import Gen.AST.Code.Combinators.Common

mkLanguagePragma :: Text -> ModulePragma Ann
mkLanguagePragma = LanguagePragma mempty . pure . mkIdent
