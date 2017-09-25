{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

module Gen.AST.Name where

import           Control.Applicative
import           Control.Lens               (over)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Writer

import qualified Data.HashMap.Strict.InsOrd as HI
import           Data.Maybe                 (fromMaybe, isJust)
import           Data.Monoid
import qualified Data.Swagger               as S
import qualified Data.Swagger.Internal      as S
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Text.Parsec                (runParser)
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Text           (Parser)

import           Gen.AST.BuiltIn
import           Gen.AST.Class
import           Gen.AST.Description
import           Gen.AST.Types

testRef :: Text
testRef = "io.k8s.kubernetes.pkg.apis.autoscaling.v2alpha1.CrossVersionObjectReference"

parseTypeName_ :: [Text] -> Parser ExternalTypeName
parseTypeName_ [] = error "Should have been called from parseKey using a nonempty list"
parseTypeName_ segments = do
  moduleName <-
    if T.null moduleName_
      then fail $ "no module name parsed from segments: " <> show segments
      else return moduleName_
  return
    ExternalName {_externalModule = moduleName, _externalName = last segments}
  where
    moduleName_ = T.intercalate "." (T.toTitle <$> init segments)

-- | Haskell identifiers can only contain [alpha|num|underscore|quote].
parseIdentifierChar :: Parser Char
parseIdentifierChar = alphaNum <|> fmap (const '_') (noneOf ".")

parseRef :: Parser ExternalTypeName
parseRef = do
  _ <- string "io.k8s."
  segments <- many1 parseIdentifierChar `sepBy1` char '.'
  eof
  parseTypeName_ . fmap T.pack $ segments

doParse :: (MonadASTError m) => Parser a -> Text -> m a
doParse parser text = case runParser parser () "" text of
  Left err  -> throwASTError ("doParse on '" <> text <> "' failed: ") err
  Right val -> return val

{- |
The only required ExternalTypeNames are those referenced in the Swagger spec.
Therefore, extended ExternalTypeNames cannot be required.
-}
extendTypeName :: Text -> Optional ExternalTypeName -> Optional ExternalTypeName
extendTypeName extension = degradeOptional . fmap (over externalName (<> "_" <> extension))

extendTypeName' :: Show a => a -> Optional ExternalTypeName -> Optional ExternalTypeName
extendTypeName' = extendTypeName . T.pack . show

referencedTypeName
  :: (MonadAST m)
  => Optional ExternalTypeName
  -> S.Referenced S.Schema
  -> m (TypeName, Maybe Description) -- ^ name and description of the referenced type
referencedTypeName typeName (S.Ref ref) = do
  refTypeName <- fromExternalTypeName <$> doParse parseRef (S.getReference ref)
  mkNewtype' typeName Nothing refTypeName -- TODO: S.Ref should be able to hold a description.
referencedTypeName typeName (S.Inline schema) = schemaTypeName typeName schema

keyedTypeName :: (MonadASTError m) => Text -> m ExternalTypeName
keyedTypeName = doParse parseRef

itemsSchemaTypeName
  :: (MonadAST m)
  => Optional ExternalTypeName
  -> S.SwaggerItems 'S.SwaggerKindSchema
  -> m (TypeName, Maybe Description) -- ^ name and description of the items for an array type
itemsSchemaTypeName typeName (S.SwaggerItemsPrimitive _ paramSchema) -- Kubernetes doesn't use the collectionFormat key. (csv by default)
 = paramSchemaTypeName typeName Nothing paramSchema
itemsSchemaTypeName typeName (S.SwaggerItemsObject ref) = referencedTypeName typeName ref
itemsSchemaTypeName typeName (S.SwaggerItemsArray refs) = do
  namesAndDescriptions <- mapM f indexedRefs
  let descriptions = snd <$> namesAndDescriptions
      tupleDescription = if any isJust descriptions
        then Just . mergeDescriptions $ extractDescriptions descriptions
        else Nothing
  mkNewtype' typeName tupleDescription $ TupleName $ fst <$> namesAndDescriptions
  where
    f (index, ref) = referencedTypeName (extendTypeName' index typeName) ref
    indexedRefs = zip [1 ..] refs
    -- Doing things with descriptions:
    decorateWithIndex :: (Show a) => Description -> a -> Description
    d `decorateWithIndex` i = labelDescription ("(" <> T.pack (show i) <> ")") d
    mergeDescriptions :: [Description] -> Description
    mergeDescriptions ds = foldl1 appendDescription' indexedDs
      where
        indexedDs :: [Description]
        indexedDs = zipWith decorateWithIndex ds [1 ..]
    extractDescriptions :: [Maybe Description] -> [Description]
    extractDescriptions = fmap $ fromMaybe (Description "<no description>")

-- | Yields the TypeName to use and writes the newtype it (maybe) created.
mkNewtype'
  :: (MonadAST m)
  => Optional ExternalTypeName
  -> Maybe Description
  -> TypeName
  -> m (TypeName, Maybe Description) -- ^ passes through the description it was given (for convenience)
mkNewtype' (Optional _) description value = return (value, description)
mkNewtype' (Required name) description value = do
  tell . pure . Left $
    Newtype
    {_newtypeName = name, _newtypeValue = value, _newtypeDescription = _descriptionText <$> description }
  return . (,description) . fromExternalTypeName $ name

paramSchemaTypeName
  :: (MonadAST m)
  => Optional ExternalTypeName
  -> Maybe Description
  -> S.ParamSchema 'S.SwaggerKindSchema
  -> m (TypeName, Maybe Description)
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
          let itemsDescription = labelDescription "(items:)" <$> itemsDescription_
              arrayDescription = description `nestDescription` itemsDescription
              arrayName = ArrayName itemsName
          mkNewtype' typeName arrayDescription arrayName
    S.SwaggerNull -> newtypify nullName
    S.SwaggerObject ->
      throwASTError "unexpected type 'object' in paramSchema: " paramSchema
  where
    newtypify = mkNewtype' typeName description

-- | This declaration is a dictionary (JSON "object").
objectTypeName
  :: (MonadAST m)
  => Optional ExternalTypeName
  -> Maybe Description
  -> [S.ParamName]
  -> HI.InsOrdHashMap Text (S.Referenced S.Schema)
  -> Maybe (S.Referenced S.Schema)
  -> m (TypeName, Maybe Description) -- ^ passes the description through for convenience
objectTypeName typeName_ description requiredProperties properties additionalProperties =
  pushASTError "objectTypeName" $ do
    fields <- mapM fieldify $ HI.toList properties
    addlFields <- traverse toAddlFields additionalProperties
    let typeName = unOptional typeName_
    tellData
      Data
      { _dataName = typeName
      , _dataFields = fields
      , _dataAddlFields = addlFields
      , _dataDescription = _descriptionText <$> description
      }
    return (fromExternalTypeName typeName, description)
  where
    fieldify (name, ref) =
      pushASTError ("fieldify", (name, ref)) $ do
        (fieldTypeName_, fieldDescription) <-
          referencedTypeName (extendTypeName name typeName_) ref
        let fieldTypeName = fieldTypeName_
            fieldRequired = name `elem` requiredProperties
        return
          Field
          { _fieldName = name
          , _fieldType = fieldTypeName
          , _fieldDescription = _descriptionText <$> fieldDescription
          , _fieldRequired = fieldRequired
          }
    toAddlFields ref =
      pushASTError ("toAddlFields", ref) $ do
        (fieldTypeName, fieldDescription) <-
          referencedTypeName (extendTypeName "addlProps" typeName_) ref
        return
          AddlFields
          { _addlFieldsType = fieldTypeName
          , _addlFieldsDescription = _descriptionText <$> fieldDescription
          }

schemaTypeName :: (MonadAST m) => Optional ExternalTypeName -> S.Schema -> m (TypeName, Maybe Description)
schemaTypeName typeName schema@S.Schema {..} =
  pushASTError ("schemaTypeName", schema) $
  case S._paramSchemaType _schemaParamSchema of
    S.SwaggerObject -> objectTypeName typeName (Description <$> _schemaDescription) _schemaRequired _schemaProperties _schemaAdditionalProperties
    _ -> paramSchemaTypeName typeName (Description <$> _schemaDescription) _schemaParamSchema
