{-# LANGUAGE TupleSections #-}
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

import Data.Maybe (isJust, fromMaybe)
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

newtype Description = Description { _descriptionContents :: Maybe Text } deriving (Show)
emptyDescription = Description Nothing

labelDescription :: Text -> Description -> Description
labelDescription label Description{..} =
  Description $ (\x -> label <> " " <> x) <$> _descriptionContents

appendDescription :: Description -> Description -> Description
appendDescription (Description Nothing) (Description Nothing) = emptyDescription
appendDescription d0 (Description Nothing) = d0
appendDescription (Description Nothing) d1 = d1
appendDescription (Description (Just d0)) (Description (Just d1)) =
  Description . Just $ d0 <> " | " <> d1

testRef :: Text
testRef = "#/definitions/io.k8s.kubernetes.pkg.apis.autoscaling.v2alpha1.CrossVersionObjectReference"

mkTypeName :: [Text] -> ExternalTypeName
mkTypeName [] = error "Should have been called from parseKey using a nonempty list"
mkTypeName segments =
  ExternalName {_externalModule = moduleName, _externalName = last segments}
  where
    moduleName_ = T.intercalate period (T.toTitle <$> init segments)
    moduleName =
      if T.null moduleName_
        then Nothing
        else Just moduleName_
    period = T.pack "."

parseRef :: Parser ExternalTypeName
parseRef = do
  _ <- string "io.k8s."
  segments <- many1 alphaNum `sepBy1` char '.'
  return . mkTypeName . fmap T.pack $ segments

doParse :: (MonadASTError m) => Parser a -> Text -> m a
doParse parser text = case runParser parser () "" text of
  Left err  -> throwASTError ("doParse on '" <> text <> "' failed: ") err
  Right val -> return val

referencedTypeName
  :: (MonadAST m)
  => Optional ExternalTypeName -> S.Referenced S.Schema -> m (TypeName, Description)
referencedTypeName typeName (S.Ref ref) = do
  refTypeName <- fromExternalTypeName <$> doParse parseRef (S.getReference ref)
  (,emptyDescription) <$> mkNewtype' typeName emptyDescription refTypeName
referencedTypeName typeName (S.Inline schema) = schemaTypeName typeName schema

keyedTypeName :: (MonadASTError m) => Text -> m ExternalTypeName
keyedTypeName = doParse parseRef

{- |
The only required ExternalTypeNames are those referenced in the Swagger spec.
Therefore, extended ExternalTypeNames cannot be required.
-}
extendTypeName :: Text -> Optional ExternalTypeName -> Optional ExternalTypeName
extendTypeName extension = degradeOptional . fmap (over externalName (<> "_" <> extension))

extendTypeName' :: Show a => a -> Optional ExternalTypeName -> Optional ExternalTypeName
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
intOrStringName = SimpleName builtInNewtypesModule "IntOrString"
unitName = SimpleName Nothing "()"

itemsSchemaTypeName
  :: (MonadAST m)
  => Optional ExternalTypeName
  -> S.SwaggerItems 'S.SwaggerKindSchema
  -> m (TypeName, Description)
itemsSchemaTypeName typeName (S.SwaggerItemsPrimitive _ paramSchema) -- Kubernetes doesn't use the collectionFormat key. (csv by default)
 = paramSchemaTypeName typeName emptyDescription paramSchema
itemsSchemaTypeName typeName (S.SwaggerItemsObject ref) = referencedTypeName typeName ref
itemsSchemaTypeName typeName (S.SwaggerItemsArray refs) = do
  namesAndDescriptions <- mapM f indexedRefs
  tupleTypeName <-
    mkNewtype' typeName emptyDescription $
    TupleName $ fst <$> namesAndDescriptions
  let descriptions = snd <$> namesAndDescriptions
      tupleDescription = if any (isJust . _descriptionContents) descriptions
        then Description . Just . mergeDescriptions $ extractDescriptions descriptions
        else emptyDescription
  return (tupleTypeName, tupleDescription)
  where
    f (index, ref) = referencedTypeName (extendTypeName' index typeName) ref
    indexedRefs = zip [1 ..] refs
    -- Doing things with descriptions:
    d `decorateWithIndex` i = "(" <> T.pack (show i) <> ")" <> d <> " "
    d0 `dConcat` d1 = d0 <> " | " <> d1
    mergeDescriptions :: [Text] -> Text
    mergeDescriptions ds = foldl1 dConcat indexedDs
      where
        indexedDs = zipWith decorateWithIndex ds [1 ..]
    extractDescriptions :: [Description] -> [Text]
    extractDescriptions = fmap $ fromMaybe "<no description>" . _descriptionContents

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
  | format == "date-time" = return dateTimeName
  | format == "password" = return passwordName
  | format == "int-or-string" = return intOrStringName
  | otherwise = throwASTError "unrecognized string format: " format

-- | Yields the TypeName to use and writes the newtype it (maybe) created.
mkNewtype'
  :: (MonadAST m)
  => Optional ExternalTypeName -> Description -> TypeName -> m TypeName
mkNewtype' (Optional _) _ value = return value -- TODO: warn if description is discarded?
mkNewtype' (Required name) Description{..} value = do
  tell . pure . Left $
    Newtype
    {_newtypeName = name, _newtypeValue = value, _newtypeDescription = _descriptionContents }
  return . fromExternalTypeName $ name

paramSchemaTypeName
  :: (MonadAST m)
  => Optional ExternalTypeName
  -> Description
  -> S.ParamSchema 'S.SwaggerKindSchema
  -> m (TypeName, Description)
paramSchemaTypeName typeName description paramSchema@S.ParamSchema {..} =
  pushASTError ("paramSchemaTypeName", paramSchema) $
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
        Just items -> do
          (itemsName, itemsDescription_) <- itemsSchemaTypeName (extendTypeName "items" typeName) items
          let itemsDescription = labelDescription "(items:)" itemsDescription_
              arrayDescription = description `appendDescription` itemsDescription
              arrayName = ArrayName itemsName
          (,arrayDescription) <$> mkNewtype' typeName arrayDescription arrayName
    S.SwaggerNull -> newtypify unitName
    S.SwaggerObject ->
      throwASTError "unexpected type 'object' in paramSchema: " paramSchema
  where
    newtypify = fmap (,description) . mkNewtype' typeName description

additionalPropertiesTypeName
  :: (MonadAST m)
  => Optional ExternalTypeName
  -> S.Referenced S.Schema -- ^ dictionary value type (aka additionalProperties)
  -> m (TypeName, Description)
additionalPropertiesTypeName typeName ref =
  pushASTError "additionalPropertiesTypeName" $ do
    (valueTypeName, valueDescription) <-
      referencedTypeName (extendTypeName "value" typeName) ref
    let dictName = DictionaryName valueTypeName
        dictDescription = (labelDescription "(value)" valueDescription)
    (,dictDescription) <$> mkNewtype' typeName dictDescription dictName

-- TODO: description
-- | This declaration is a dictionary (JSON "object").
objectTypeName
  :: (MonadAST m)
  => Optional ExternalTypeName
  -> Description
  -> [S.ParamName]
  -> HI.InsOrdHashMap Text (S.Referenced S.Schema)
  -> Maybe (S.Referenced S.Schema)
  -> m TypeName
objectTypeName typeName description requiredProperties properties additionalProperties =
  pushASTError "objectTypeName" $ do
  props <- mapM fieldify $ HI.toList properties
  fields <- case additionalProperties of
    Nothing             -> return props
    Just addlPropsValue -> (:props) <$> fieldify' addlPropsValue
  let typeName' = fromExternalTypeName . unOptional $ typeName
  tellData $ Data { _dataName = typeName'
                  , _dataFields = fields
                  , _dataDescription = _descriptionContents description }
  return typeName'
  where fieldify (name, ref) = pushASTError ("fieldify", (name, ref)) $ do
          (fieldTypeName_, fieldDescription) <- referencedTypeName (extendTypeName name typeName) ref
          let fieldTypeName = if name `elem` requiredProperties
                then fieldTypeName_
                else MaybeName fieldTypeName_
          return $ Field { _fieldName = Right name
                         , _fieldType = fieldTypeName
                         , _fieldDescription = _descriptionContents fieldDescription }
        fieldify' ref = pushASTError ("fieldify'", ref) $ do
          (fieldTypeName, fieldDescription) <- additionalPropertiesTypeName (extendTypeName "addlProps" typeName) ref
          return $ Field { _fieldName = Left AdditionalProperties
                         , _fieldType = fieldTypeName
                         , _fieldDescription = _descriptionContents fieldDescription }

schemaTypeName :: (MonadAST m) => Optional ExternalTypeName -> S.Schema -> m (TypeName, Description)
schemaTypeName typeName schema@S.Schema {..} =
  pushASTError ("schemaTypeName", schema) $
  case S._paramSchemaType _schemaParamSchema of
    S.SwaggerObject -> (,emptyDescription) <$> objectTypeName typeName (Description _schemaDescription) _schemaRequired _schemaProperties _schemaAdditionalProperties
    _ -> paramSchemaTypeName typeName (Description _schemaDescription) _schemaParamSchema
