{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Gen.AST.Name where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Except

import qualified Data.HashMap.Strict.InsOrd as HI
import           Data.Monoid
import qualified Data.Swagger               as S
import qualified Data.Swagger.Internal      as S
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Text           (Parser)

import           Gen.AST.Error

-- #/definitions/io.k8s.apimachinery.pkg.util.intstr.IntOrString
-- #/definitions/io.k8s.apimachinery.pkg.apis.meta.v1.ListMeta
-- #/definitions/io.k8s.kubernetes.pkg.apis.extensions.v1beta1.HTTPIngressRuleValue
-- #/definitions/io.k8s.kubernetes.pkg.apis.rbac.v1alpha1.PolicyRule
-- #/definitions/io.k8s.kubernetes.pkg.apis.autoscaling.v2alpha1.CrossVersionObjectReference
-- int32
-- int64
-- string

testRef :: Text
testRef = "#/definitions/io.k8s.kubernetes.pkg.apis.autoscaling.v2alpha1.CrossVersionObjectReference"

data TypeName where
  ArrayName :: TypeName -> TypeName
  TupleName :: [TypeName] -> TypeName
  SimpleName :: Maybe Text -> Text -> TypeName
  deriving (Show)

makeLenses ''TypeName

mkTypeName :: [Text] -> TypeName
mkTypeName [] = error "Should have been called from parseKey using a nonempty list"
mkTypeName segments = SimpleName moduleName (last segments)
  where
    moduleName_ = T.intercalate period (T.toTitle <$> init segments)
    moduleName =
      if T.null moduleName_
        then Nothing
        else Just moduleName_
    period = T.pack "."

parseRef :: Parser TypeName
parseRef = string "#/definitions/" >> parseKey

parseKey :: Parser TypeName
parseKey = do
  _ <- string "io.k8s."
  segments <- many1 alphaNum `sepBy1` char '.'
  return . mkTypeName . fmap T.pack $ segments

doParse :: Parser a -> Text -> ASTExcept a
doParse parser text = case runParser parser () "" text of
  Left err  -> throwASTError ("doParse on '" <> text <> "' failed: ") err
  Right val -> return val

referencedTypeName :: S.Referenced S.Schema -> ASTExcept TypeName
referencedTypeName (S.Ref ref)       = doParse parseRef $ S.getReference ref
referencedTypeName (S.Inline schema) = throwASTError "referencedTypeName called with an inline schema: " schema

keyedTypeName :: Text -> ASTExcept TypeName
keyedTypeName = doParse parseKey

-- TODO: Create newtypes for Kubernetes.Types.Base types.
builtInNewtypesModule :: Maybe Text
builtInNewtypesModule = Just "Kubernetes.Types.Base"

-- TODO: There are other "format"s as well--i.e. "email" for "string"
int32Name = SimpleName (Just "Data.Int") "Int32"
int64Name = SimpleName (Just "Data.Int") "Int64"
floatName = SimpleName Nothing "Float"
doubleName = SimpleName Nothing "Double"
textName = SimpleName (Just "Data.Text") "Text"
base64Name = SimpleName builtInNewtypesModule "Base64String"
octetsName = SimpleName (Just "Data.ByteString") "ByteString"
boolName = SimpleName Nothing "Bool"
dateName = SimpleName (Just "Data.Time") "Day"
dateTimeName = SimpleName (Just "Data.Time") "UTCTime"
passwordName = SimpleName builtInNewtypesModule "Password"
unitName = SimpleName Nothing "()"

itemsSchemaTypeName :: S.SwaggerItems 'S.SwaggerKindSchema -> ASTExcept TypeName
itemsSchemaTypeName (S.SwaggerItemsPrimitive _ paramSchema) -- Kubernetes doesn't use the collectionFormat key. (csv by default)
 = paramSchemaTypeName paramSchema
itemsSchemaTypeName (S.SwaggerItemsObject ref) = referencedTypeName ref
itemsSchemaTypeName (S.SwaggerItemsArray refs) = TupleName <$> mapM referencedTypeName refs

integerTypeName :: Maybe S.Format -> ASTExcept TypeName
integerTypeName Nothing = throwASTError "missing format for integer" ()
integerTypeName (Just format)
  | format == "int32" = return int32Name
  | format == "int64" = return int64Name
  | otherwise = throwASTError "unrecognized integer format: " format

numberTypeName :: Maybe S.Format -> ASTExcept TypeName
numberTypeName Nothing = throwASTError "missing format for number" ()
numberTypeName (Just format)
  | format == "float" = return floatName
  | format == "double" = return doubleName
  | otherwise = throwASTError "unrecognized number format: " format

stringTypeName :: Maybe S.Format -> ASTExcept TypeName
stringTypeName Nothing = return textName
stringTypeName (Just format)
  | format == "byte" = return base64Name
  | format == "binary" = return octetsName
  | format == "date" = return dateName
  | format == "dateTime" = return dateTimeName
  | format == "password" = return passwordName
  | otherwise = throwASTError "unrecognized string format: " format

-- TODO: This is insufficient. Object type needs "properties" and "additionalProperties" to determine a TypeName.
-- TODO: (UNLIKELY) It's also possible that a property of an object may need a "data" definition.
--       We may need a write monad to keep track of these new Types.
--       NOTE: I don't imagine an autogenerated swagger schema would contain nested definitions like that.
paramSchemaTypeName :: S.ParamSchema 'S.SwaggerKindSchema -> ASTExcept TypeName
paramSchemaTypeName paramSchema@S.ParamSchema{..} = pushASTError paramSchema $
  case _paramSchemaType of
    S.SwaggerInteger -> integerTypeName $ _paramSchemaFormat
    S.SwaggerNumber  -> numberTypeName $ _paramSchemaFormat
    S.SwaggerString  -> stringTypeName $ _paramSchemaFormat
    S.SwaggerBoolean -> return boolName
    S.SwaggerArray   -> case _paramSchemaItems of
      Nothing -> throwASTError "array paramSchema should have paramSchemaItems: " paramSchema
      Just items -> ArrayName <$> itemsSchemaTypeName items
    S.SwaggerNull    -> return unitName
    S.SwaggerObject  -> throwASTError "unexpected type 'object' in paramSchema: " paramSchema

objectTypeName
  :: HI.InsOrdHashMap Text (S.Referenced S.Schema)
  -> Maybe (S.Referenced S.Schema)
  -> ASTExcept TypeName
objectTypeName properties additionalProperties
  | HI.null properties =  case additionalProperties of
      Nothing -> throwASTError "this object has no additional properties: " ()
      Just ref -> referencedTypeName ref
  | otherwise = throwASTError "this object needs a new type declaration: " properties
