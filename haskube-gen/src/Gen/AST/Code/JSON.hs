{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gen.AST.Code.JSON where

import           Language.Haskell.Exts

import           Data.Foldable            (foldl')
import           Data.Maybe               (isNothing, maybeToList)
import           Data.Monoid
import           Data.Text                (Text, unpack)

import qualified Gen.AST.BuiltIn          as G
import           Gen.AST.Code.Combinators
import           Gen.AST.Code.Common
import           Gen.AST.Code.Data
import           Gen.AST.Code.Types
import qualified Gen.AST.Types            as G

aesonPrefix :: Text
aesonPrefix = "AE"

maybePrefix :: Text
maybePrefix = "Data.Maybe"

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

hashMapImport :: ImportDecl Ann
hashMapImport =
  ImportDecl
  { importAnn = mempty
  , importModule = mkModuleName "Data.HashMap.Strict"
  , importQualified = True
  , importSrc = False
  , importSafe = False
  , importPkg = Nothing
  , importAs = Just $ mkModuleName "Data.HashMap.Strict"
  , importSpecs = Nothing
  }

maybeImport :: ImportDecl Ann
maybeImport =
  ImportDecl
  { importAnn = mempty
  , importModule = mkModuleName "Data.Maybe"
  , importQualified = True
  , importSrc = False
  , importSafe = False
  , importPkg = Nothing
  , importAs = Just $ mkModuleName maybePrefix
  , importSpecs = Nothing
  }

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

-- | Data.Aeson's "parse optional object property" operator
xDotColonQuest :: QOp Ann
xDotColonQuest = mkQVarOp_ aesonPrefix ".:?"

{-
-- | Data.Aeson's "to JSON key-value pair" operator
xDotEquals :: QOp Ann
xDotEquals = mkQVarOp_ aesonPrefix ".="
-}

xMaybePair :: Exp Ann
xMaybePair = mkVarExp G.builtInNewtypesModule' "maybePair"

xJustPair :: Exp Ann
xJustPair = mkVarExp G.builtInNewtypesModule' "justPair"

-- | <$>
xFancyDollar :: QOp Ann
xFancyDollar = mkQVarOp_' "<$>"

-- | <*>
xFancyStar :: QOp Ann
xFancyStar = mkQVarOp_' "<*>"

xParseAddlProps :: Exp Ann
xParseAddlProps = mkVarExp G.builtInNewtypesModule' "parseAddlProps"

xAddAddlProps :: Exp Ann
xAddAddlProps = mkVarExp G.builtInNewtypesModule' "addAddlProps"

{- | String literal to be used to index into a JSON object.
It should be unaltered from the OpenAPI spec.
-}
mkFieldString :: Text -> Exp Ann
mkFieldString = mkString

xTypeMismatch :: Text -> Exp Ann
xTypeMismatch conName =
  mkApp (mkApp (mkVarExp aesonPrefix "typeMismatch") (mkString conName)) xInvalid

-- | AE.object
xObject :: Exp Ann
xObject = mkVarExp aesonPrefix "object"

xCatMaybes :: Exp Ann
xCatMaybes = mkVarExp maybePrefix "catMaybes"

{- |
essentially, field -> "obj .: fieldName"
-}
xParseField :: G.Field -> Exp Ann
xParseField G.Field {..} =
  mkInfixApp
    xObj
    (if _fieldRequired
       then xDotColon
       else xDotColonQuest) $
  mkFieldString _fieldName

{- |
essentially, addlProps -> "parseAddlProps [normalFieldNames] obj"
-}
xParseAddlFields :: [Text] -> G.AddlFields -> Exp Ann
xParseAddlFields normalFieldNames_ G.AddlFields {..} =
  mkApp (mkApp xParseAddlProps normalFieldNames) xObj
  where
    normalFieldNames = List mempty $ mkFieldString <$> normalFieldNames_

{- |
object ["name" .= _name, "age" .= _age]
-}
xPropsToJSON :: [(Text, Bool)] -> Exp Ann
xPropsToJSON props =
  mkApp (mkInfixApp xObject xDot xCatMaybes) (List mempty $ f <$> props)
  where
    f (prop, required) =
      mkApp
        (mkApp
           (if required
              then xJustPair
              else xMaybePair) $
         mkString prop)
        (mkFieldName prop)

xAddlPropsToJSON :: Exp Ann -- ^ value expression
                 -> Exp Ann
xAddlPropsToJSON = mkApp (mkApp xAddAddlProps xAddlProps)

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

mkDataParseJSONRHS :: Text -> [G.Field] -> Maybe G.AddlFields -> Rhs Ann
mkDataParseJSONRHS conName [] Nothing =
  UnGuardedRhs mempty (mkApp (mkVarExp' "return") (mkVarExp' conName))
mkDataParseJSONRHS conName fields addlFields = UnGuardedRhs mempty exp
  where exp = foldl' f (xCon conName) $ zip ops propParsers
        fieldParsers = xParseField <$> fields
        addlFieldsParser = maybeToList $ xParseAddlFields normalFieldNames <$> addlFields
        propParsers = fieldParsers <> addlFieldsParser
        ops = xFancyDollar:repeat xFancyStar
        f expHead (op, parser) = mkInfixApp expHead op parser
        normalFieldNames = G._fieldName <$> fields

mkDataParseJSON :: G.Data -> InstDecl Ann
mkDataParseJSON G.Data {..} = InsDecl mempty $ FunBind mempty [match0, match1]
  where
    match0 =
      Match
        mempty
        nParseJSON
        [pObj]
        (mkDataParseJSONRHS conName _dataFields _dataAddlFields)
        Nothing
    match1 =
      Match
        mempty
        nParseJSON
        [pInvalid]
        (UnGuardedRhs mempty $ xTypeMismatch conName)
        Nothing
    conName = G._externalName _dataName

mkDataToJSONRHS :: [G.Field] -> Maybe G.AddlFields -> Rhs Ann
mkDataToJSONRHS fields addlFields_ = UnGuardedRhs mempty exp
  where
    exp =
      case addlFields_ of
        Nothing         -> propsExp
        Just addlFields -> xAddlPropsToJSON propsExp
    propsExp = xPropsToJSON $ f <$> fields
    f G.Field {..} = (_fieldName, _fieldRequired)

mkDataToJSON_ :: G.Data -> InstDecl Ann
mkDataToJSON_ G.Data {..} = InsDecl mempty $ FunBind mempty [match]
  where
    match =
      Match
        mempty
        nToJSON
        [pat]
        (mkDataToJSONRHS _dataFields _dataAddlFields)
        Nothing
    conName = G._externalName _dataName
    pat = if null _dataFields && isNothing _dataAddlFields then pCon conName else pRecordWildcard conName

qnFromJSON :: QName Ann
qnFromJSON = mkQual aesonPrefix "FromJSON"

qnToJSON :: QName Ann
qnToJSON = mkQual aesonPrefix "ToJSON"

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

-- TODO: override default toEncoding
mkDataFromJSON :: G.Data -> Decl Ann
mkDataFromJSON aData =
  InstDecl mempty Nothing (mkInstRule qnFromJSON aType) $
  Just [mkDataParseJSON aData]
  where aType = tData aData

-- TODO: override default toEncoding
mkDataToJSON :: G.Data -> Decl Ann
mkDataToJSON aData =
  InstDecl mempty Nothing (mkInstRule qnToJSON aType) $
  Just [mkDataToJSON_ aData]
  where aType = tData aData

mkJSONs :: [G.Type] -> [Decl Ann]
mkJSONs = foldl' f []
  where
    f insts (Left aNewtype) =
      mkNewtypeFromJSON aNewtype : mkNewtypeToJSON aNewtype : insts
    f insts (Right aData) = mkDataFromJSON aData : mkDataToJSON aData : insts
