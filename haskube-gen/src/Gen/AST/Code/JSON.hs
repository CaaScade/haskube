{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gen.AST.Code.JSON where

import           Language.Haskell.Exts

import           Data.Either           (either, rights)
import           Data.Foldable         (foldl')
import           Data.Text             (Text, unpack)

import qualified Gen.AST.BuiltIn       as G
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

aesonImports :: [ImportDecl Ann]
aesonImports =
  [ ImportDecl
    { importAnn = mempty
    , importModule = mkModuleName "Data.Aeson"
    , importQualified = True
    , importSrc = False
    , importSafe = False
    , importPkg = Nothing
    , importAs = Just $ mkModuleName aesonPrefix
    , importSpecs = Nothing
    }
  , ImportDecl
    { importAnn = mempty
    , importModule = mkModuleName "Data.Aeson.Types"
    , importQualified = True
    , importSrc = False
    , importSafe = False
    , importPkg = Nothing
    , importAs = Just $ mkModuleName aesonPrefix
    , importSpecs = Nothing
    }
  ]

mkVarExp :: Text -> Text -> Exp Ann
mkVarExp moduleName = Var mempty . mkQual moduleName

mkVarExp' :: Text -> Exp Ann
mkVarExp' = Var mempty . mkUnqual

xDot :: QOp Ann
xDot = mkQVarOp_' "."

xFmap :: Exp Ann
xFmap = mkVarExp' "fmap"

nParseJSON :: Name Ann
nParseJSON = mkIdent "parseJSON"

xParseJSON :: Exp Ann
xParseJSON = Var mempty $ mkQual aesonPrefix "parseJSON"

nToJSON :: Name Ann
nToJSON = mkIdent "toJSON"

xToJSON :: Exp Ann
xToJSON = Var mempty $ mkQual aesonPrefix "toJSON"

-- | Create an expression with an unqualified constructor name.
xCon :: Text -> Exp Ann
xCon = mkVarExp'

-- | Expression for a variable called "obj"
xObj :: Exp Ann
xObj = mkVarExp' "obj"

-- | Pattern "(AE.Object obj)"
pObj :: Pat Ann
pObj = PApp mempty (mkQual aesonPrefix "Object") [obj]
  where obj = PVar mempty $ mkIdent "obj"

-- | Expression for a variable called "invalid"
xInvalid :: Exp Ann
xInvalid = mkVarExp' "invalid"

-- | Pattern "invalid"
pInvalid :: Pat Ann
pInvalid = PVar mempty $ mkIdent "invalid"

-- | Data.Aeson's "parse object property" operator
xDotColon :: QOp Ann
xDotColon = mkQVarOp_ aesonPrefix ".:"

-- | <$>
xFancyDollar :: QOp Ann
xFancyDollar = mkQVarOp_' "<$>"

-- | <*>
xFancyStar :: QOp Ann
xFancyStar = mkQVarOp_' "<*>"

mkApp :: Exp Ann -> Exp Ann -> Exp Ann
mkApp = App mempty

mkInfixApp :: Exp Ann -> QOp Ann -> Exp Ann -> Exp Ann
mkInfixApp = InfixApp mempty

-- | Variable for a field (like it's been record-wildcarded)
mkFieldName :: Text -- ^ field name without the leading underscore
            -> Exp Ann
mkFieldName = mkVarExp' . toRecordFieldName

xParseAddlProps :: Exp Ann
xParseAddlProps = mkVarExp G.builtInNewtypesModule' "parseAddlProps"

-- | Make a string literal.
mkString :: Text -> Exp Ann
mkString text_ = Lit mempty $ String mempty text text
  where text = unpack text_

mkFieldString :: Text -> Exp Ann
mkFieldString = mkString . toRecordFieldName

xTypeMismatch :: Text -> Exp Ann
xTypeMismatch conName =
  mkApp (mkApp (mkVarExp aesonPrefix "typeMismatch") (mkString conName)) xInvalid

{- |
essentially, field -> "obj .: fieldName"
OR addlProps -> "parseAddlProps [normalFieldNames] obj"
-}
xParseFields :: [G.Field] -> [Exp Ann]
xParseFields fields = f <$> fields
  where
    f G.Field {..} =
      case _fieldName of
        Left G.AdditionalProperties ->
          mkApp (mkApp xParseAddlProps normalFieldNames) xObj
        Right fieldName -> mkInfixApp xObj xDotColon $ mkFieldString fieldName
    normalFieldNames = List mempty $ mkFieldString <$> (rights $ G._fieldName <$> fields)

mkNewtypeParseJSONRHS :: Text -> Rhs Ann
mkNewtypeParseJSONRHS conName = UnGuardedRhs mempty exp
  where exp = mkInfixApp (mkApp xFmap $ xCon conName) xDot xParseJSON

mkNewtypeParseJSON :: G.Newtype -> InstDecl Ann
mkNewtypeParseJSON G.Newtype{..} = InsDecl mempty $ FunBind mempty [match]
  where match = Match mempty nParseJSON [] (mkNewtypeParseJSONRHS conName) Nothing
        conName = G._externalName _newtypeName

mkNewtypeToJSONRHS :: Text -> Rhs Ann
mkNewtypeToJSONRHS conName = UnGuardedRhs mempty exp
  where exp = mkInfixApp xToJSON xDot (mkVarExp' $ toNewtypeFieldName conName)

mkNewtypeToJSON_ :: G.Newtype -> InstDecl Ann
mkNewtypeToJSON_ G.Newtype{..} = InsDecl mempty $ FunBind mempty [match]
  where match = Match mempty nToJSON [] (mkNewtypeToJSONRHS conName) Nothing
        conName = G._externalName _newtypeName

mkDataParseJSONRHS :: Text -> [G.Field] -> Rhs Ann
mkDataParseJSONRHS conName [] =
  UnGuardedRhs mempty (mkApp (mkVarExp' "return") (mkVarExp' conName))
mkDataParseJSONRHS conName fields = UnGuardedRhs mempty exp
  where exp = foldl' f (xCon conName) $ zip ops propParsers
        propParsers = xParseFields fields
        ops = xFancyDollar:repeat xFancyStar
        f expHead (op, parser) = mkInfixApp expHead op parser

mkDataParseJSON :: G.Data -> InstDecl Ann
mkDataParseJSON G.Data {..} = InsDecl mempty $ FunBind mempty [match0, match1]
  where
    match0 =
      Match
        mempty
        nParseJSON
        [pObj]
        (mkDataParseJSONRHS conName _dataFields)
        Nothing
    match1 =
      Match
        mempty
        nParseJSON
        [pInvalid]
        (UnGuardedRhs mempty $ xTypeMismatch conName)
        Nothing
    conName = G._externalName _dataName

qnFromJSON :: QName Ann
qnFromJSON = mkQual aesonPrefix "FromJSON"

qnToJSON :: QName Ann
qnToJSON = mkQual aesonPrefix "ToJSON"

tNewtype :: G.Newtype -> Type Ann
tNewtype G.Newtype{..} = TyCon mempty . mkUnqual $ G._externalName _newtypeName

tData :: G.Data -> Type Ann
tData G.Data{..} = TyCon mempty . mkUnqual $ G._externalName _dataName

mkNewtypeFromJSON :: G.Newtype -> Decl Ann
mkNewtypeFromJSON aNewtype =
  InstDecl mempty Nothing (mkInstRule qnFromJSON aType) $
  Just [mkNewtypeParseJSON aNewtype]
  where
    aType = tNewtype aNewtype

mkNewtypeToJSON :: G.Newtype -> Decl Ann
mkNewtypeToJSON aNewtype =
  InstDecl mempty Nothing (mkInstRule qnToJSON aType) $
  Just [mkNewtypeToJSON_ aNewtype]
  where
    aType = tNewtype aNewtype

mkDataFromJSON :: G.Data -> Decl Ann
mkDataFromJSON aData =
  InstDecl mempty Nothing (mkInstRule qnFromJSON aType) $
  Just [mkDataParseJSON aData]
  where aType = tData aData

mkJSONs :: [G.Type] -> [Decl Ann]
mkJSONs types = foldl' f [] types
  where
    f insts (Left aNewtype) =
      mkNewtypeFromJSON aNewtype : mkNewtypeToJSON aNewtype : insts
    f insts (Right aData) = mkDataFromJSON aData : insts
