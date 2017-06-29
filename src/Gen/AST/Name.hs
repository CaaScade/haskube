{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
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
import           Control.Monad.Writer

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
import           Gen.AST.Types

testRef :: Text
testRef = "#/definitions/io.k8s.kubernetes.pkg.apis.autoscaling.v2alpha1.CrossVersionObjectReference"

-- TODO: Track when this function is used. I don't think we should need it.
propertyTypeName :: MonadASTError m => TypeName -> Text -> m TypeName
propertyTypeName (SimpleName moduleName name) propertyName =
  return $ SimpleName moduleName (name <> "_" <> propertyName)
propertyTypeName typeName _ = throwASTError "we can't extend a typeName we didn't create" typeName

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

doParse :: (MonadASTError m) => Parser a -> Text -> m a
doParse parser text = case runParser parser () "" text of
  Left err  -> throwASTError ("doParse on '" <> text <> "' failed: ") err
  Right val -> return val

referencedTypeName :: (MonadAST m) => Optional ExternalTypeName -> S.Referenced S.Schema -> m TypeName
referencedTypeName typeName (S.Ref ref)       = mkNewtype' typeName =<< doParse parseRef (S.getReference ref)
referencedTypeName typeName (S.Inline schema) = schemaTypeName typeName schema

keyedTypeName :: (MonadASTError m) => Text -> m TypeName
keyedTypeName = doParse parseKey

extendTypeName :: Text -> ExternalTypeName -> ExternalTypeName
extendTypeName extension = over externalName (<> "_" <> extension)

extendTypeName' :: Show a => a -> ExternalTypeName -> ExternalTypeName
extendTypeName' = extendTypeName . T.pack . show

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

itemsSchemaTypeName :: (MonadAST m) => Optional ExternalTypeName -> S.SwaggerItems 'S.SwaggerKindSchema -> m TypeName
itemsSchemaTypeName typeName (S.SwaggerItemsPrimitive _ paramSchema) -- Kubernetes doesn't use the collectionFormat key. (csv by default)
 = paramSchemaTypeName typeName paramSchema
itemsSchemaTypeName typeName (S.SwaggerItemsObject ref) = referencedTypeName typeName ref
itemsSchemaTypeName typeName (S.SwaggerItemsArray refs) =
  mkNewtype' typeName =<< TupleName <$> mapM f indexedRefs
  where
    f (index, ref) = referencedTypeName (extendTypeName' index <$> typeName) ref
    indexedRefs = zip [1 ..] refs

integerTypeName :: (MonadASTError m) => Maybe S.Format -> m TypeName
integerTypeName Nothing = throwASTError "missing format for integer" ()
integerTypeName (Just format)
  | format == "int32" = return int32Name
  | format == "int64" = return int64Name
  | otherwise = throwASTError "unrecognized integer format: " format

numberTypeName :: (MonadASTError m) => Maybe S.Format -> m TypeName
numberTypeName Nothing = throwASTError "missing format for number" ()
numberTypeName (Just format)
  | format == "float" = return floatName
  | format == "double" = return doubleName
  | otherwise = throwASTError "unrecognized number format: " format

stringTypeName :: (MonadASTError m) => Maybe S.Format -> m TypeName
stringTypeName Nothing = return textName
stringTypeName (Just format)
  | format == "byte" = return base64Name
  | format == "binary" = return octetsName
  | format == "date" = return dateName
  | format == "dateTime" = return dateTimeName
  | format == "password" = return passwordName
  | otherwise = throwASTError "unrecognized string format: " format

--TODO: description
-- | Yields the TypeName to use and writes the newtype it (maybe) created.
mkNewtype' :: (MonadAST m) => Optional ExternalTypeName -> TypeName -> m TypeName
mkNewtype' (Optional _) value = return value
mkNewtype' (Required name) value = do
  tell . pure . Left $
    Newtype
    {_newtypeName = name, _newtypeValue = value, _newtypeDescription = Nothing}
  return . fromExternalTypeName $ name

paramSchemaTypeName :: (MonadAST m) => Optional ExternalTypeName -> S.ParamSchema 'S.SwaggerKindSchema -> m TypeName
paramSchemaTypeName typeName paramSchema@S.ParamSchema {..} =
  pushASTError paramSchema $
  case _paramSchemaType of
    S.SwaggerInteger -> newtypify =<< integerTypeName _paramSchemaFormat
    S.SwaggerNumber -> newtypify =<< numberTypeName _paramSchemaFormat
    S.SwaggerString -> newtypify =<< stringTypeName _paramSchemaFormat
    S.SwaggerBoolean -> newtypify boolName
    S.SwaggerArray ->
      case _paramSchemaItems of
        Nothing ->
          throwASTError
            "array paramSchema should have paramSchemaItems: "
            paramSchema
        Just items ->
          newtypify =<<
          ArrayName <$>
          itemsSchemaTypeName (extendTypeName "items" <$> typeName) items
    S.SwaggerNull -> newtypify unitName
    S.SwaggerObject ->
      throwASTError "unexpected type 'object' in paramSchema: " paramSchema
  where
    newtypify = mkNewtype' typeName

additionalPropertiesTypeName
  :: (MonadAST m)
  => Optional ExternalTypeName
  -> S.Referenced S.Schema -- ^ dictionary value type (aka additionalProperties)
  -> m TypeName
additionalPropertiesTypeName typeName ref =
  mkNewtype' typeName =<< DictionaryName <$> referencedTypeName (extendTypeName "value" <$> typeName) ref

-- TODO: description
-- | This declaration is a dictionary (JSON "object").
objectTypeName
  :: (MonadAST m)
  => Optional ExternalTypeName
  -> HI.InsOrdHashMap Text (S.Referenced S.Schema)
  -> Maybe (S.Referenced S.Schema)
  -> m TypeName
objectTypeName typeName properties additionalProperties = do
  props <- mapM fieldify $ HI.toList properties
  fields <- case additionalProperties of
    Nothing -> return props
    Just addlPropsValue -> (:props) <$> fieldify' addlPropsValue
  let typeName' = fromExternalTypeName . unOptional $ typeName
  tellData $ Data { _dataName = typeName'
                  , _dataFields = fields
                  , _dataDescription = Nothing }
  return typeName'
  where fieldify (name, ref) = do
          fieldTypeName <- referencedTypeName (extendTypeName name <$> typeName) ref
          return $ Field { _fieldName = Right name
                         , _fieldType = fieldTypeName
                         , _fieldDescription = Nothing }
        fieldify' ref = do
          fieldTypeName <- additionalPropertiesTypeName (extendTypeName "addlProps" <$> typeName) ref
          return $ Field { _fieldName = Left AdditionalProperties
                         , _fieldType = fieldTypeName
                         , _fieldDescription = Nothing }

schemaTypeName :: (MonadAST m) => Optional ExternalTypeName -> S.Schema -> m TypeName
schemaTypeName typeName schema@S.Schema {..} =
  pushASTError schema $
  case S._paramSchemaType _schemaParamSchema of
    S.SwaggerObject -> objectTypeName typeName _schemaProperties _schemaAdditionalProperties
    _ -> paramSchemaTypeName typeName _schemaParamSchema
