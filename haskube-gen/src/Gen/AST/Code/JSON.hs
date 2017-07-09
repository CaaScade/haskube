{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gen.AST.Code.JSON where

import           Language.Haskell.Exts

import           Data.Either              (either, rights, lefts)
import           Data.Foldable            (foldl')
import           Data.Text                (Text, unpack)

import qualified Gen.AST.BuiltIn          as G
import           Gen.AST.Code.Combinators
import           Gen.AST.Code.Data
import           Gen.AST.Code.Types
import qualified Gen.AST.Types            as G

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

-- | Data.Aeson's "to JSON key-value pair" operator
xDotEquals :: QOp Ann
xDotEquals = mkQVarOp_ aesonPrefix ".="

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
    normalFieldNames =
      List mempty $ mkFieldString <$> getNormalFieldNames fields

{- |
object ["name" .= _name, "age" .= _age]
-}
xPropsToJSON :: [Text] -> Exp Ann
xPropsToJSON props =
  mkApp xObject (List mempty $ f <$> props)
  where f prop = mkInfixApp (mkString prop) xDotEquals (mkFieldName prop)

xAddlPropsToJSON :: Exp Ann -> Exp Ann
xAddlPropsToJSON valueExp =
  mkApp (mkApp xAddAddlProps xAddlProps) valueExp

getNormalFieldNames :: [G.Field] -> [Text]
getNormalFieldNames = rights . fmap G._fieldName

hasAddlPropsField :: [G.Field] -> Bool
hasAddlPropsField = not . null . lefts . fmap G._fieldName

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

mkDataToJSONRHS :: [G.Field] -> Rhs Ann
mkDataToJSONRHS fields = UnGuardedRhs mempty exp
  where exp = if hasAddlPropsField fields then xAddlPropsToJSON propsExp
              else propsExp
        propsExp = xPropsToJSON $ getNormalFieldNames fields

mkDataToJSON_ :: G.Data -> InstDecl Ann
mkDataToJSON_ G.Data {..} = InsDecl mempty $ FunBind mempty [match]
  where
    match =
      Match
        mempty
        nToJSON
        [pat]
        (mkDataToJSONRHS _dataFields)
        Nothing
    conName = G._externalName _dataName
    pat = if null _dataFields then pCon conName else pRecordWildcard conName

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
mkJSONs types = foldl' f [] types
  where
    f insts (Left aNewtype) =
      mkNewtypeFromJSON aNewtype : mkNewtypeToJSON aNewtype : insts
    f insts (Right aData) = mkDataFromJSON aData : mkDataToJSON aData : insts