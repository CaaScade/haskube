{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gen.AST.Code.JSON where

import           Language.Haskell.Exts

import Data.Either (either)
import           Data.Foldable         (foldl')
import           Data.Text             (Text)

import           Gen.AST.Code.Data
import           Gen.AST.Code.Types
import qualified Gen.AST.Types         as G

mkInstHead :: QName Ann -> Type Ann -> InstHead Ann
mkInstHead className aType =
  IHApp mempty (IHCon mempty className) aType

mkInstRule :: QName Ann -> Type Ann -> InstRule Ann
mkInstRule className aType =
  IRule mempty Nothing Nothing $ mkInstHead className aType

aesonPrefix :: Text
aesonPrefix = "AE"

aesonImport :: ImportDecl Ann
aesonImport = ImportDecl
  { importAnn = mempty
  , importModule = mkModuleName "Data.Aeson"
  , importQualified = True
  , importSrc = False
  , importSafe = False
  , importPkg = Nothing
  , importAs = Just $ mkModuleName aesonPrefix
  , importSpecs = Nothing
  }

mkVarExp' :: Text -> Exp Ann
mkVarExp' = Var mempty . mkUnqual

xDot :: QOp Ann
xDot = mkQVarOp' "."

xFmap :: Exp Ann
xFmap = mkVarExp' "fmap"

nParseJSON :: Name Ann
nParseJSON = mkIdent "parseJSON"

xParseJSON :: Exp Ann
xParseJSON = Var mempty $ mkQual aesonPrefix "parseJSON"

xCon :: Text -> Exp Ann
xCon = mkVarExp'

mkApp :: Exp Ann -> Exp Ann -> Exp Ann
mkApp = App mempty

mkInfixApp :: Exp Ann -> QOp Ann -> Exp Ann -> Exp Ann
mkInfixApp = InfixApp mempty

mkNewtypeParseJSONRHS :: Text -> Rhs Ann
mkNewtypeParseJSONRHS conName = UnGuardedRhs mempty exp
  where exp = mkInfixApp (mkApp xFmap $ xCon conName) xDot xParseJSON

mkNewtypeParseJSON :: G.Newtype -> InstDecl Ann
mkNewtypeParseJSON G.Newtype{..} = InsDecl mempty $ FunBind mempty [match]
  where match = Match mempty nParseJSON [] (mkNewtypeParseJSONRHS conName) Nothing
        conName = G._externalName _newtypeName

qnFromJSON :: QName Ann
qnFromJSON = mkQual aesonPrefix "FromJSON"

mkNewtypeFromJSON :: G.Newtype -> Decl Ann
mkNewtypeFromJSON aNewtype@G.Newtype{..} =
  InstDecl mempty Nothing (mkInstRule qnFromJSON aType) $ Just [mkNewtypeParseJSON aNewtype]
  where aType = TyCon mempty . mkUnqual $ G._externalName _newtypeName

mkFromJSONs :: [G.Type] -> [Decl Ann]
mkFromJSONs types = foldl' f [] types
  where f insts (Left aNewtype) = mkNewtypeFromJSON aNewtype:insts
        f insts (Right _) = insts
