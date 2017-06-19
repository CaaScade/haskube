{-# LANGUAGE OverloadedStrings #-}

module Gen.AST.Name where

import           Control.Applicative
import           Control.Monad

import qualified Data.Swagger           as S
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Text       (Parser)

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

data TypeName = TypeName
  { _typeNameModule :: Maybe Text
  , _typeNameName       :: Text
  } deriving (Show)

mkTypeName :: [Text] -> TypeName
mkTypeName [] = error "Should have been called from parseRef using a non-empty list"
mkTypeName segments = TypeName { _typeNameModule = if T.null moduleName then Nothing else Just moduleName
                               , _typeNameName = last segments }
  where moduleName = T.intercalate period (T.toTitle <$> init segments)
        period = T.pack "."

parseRef :: Parser TypeName
parseRef = string "#/definitions/" >> parseKey

parseKey :: Parser TypeName
parseKey = do
  _ <- string "io.k8s."
  segments <- many1 alphaNum `sepBy1` char '.'
  return . mkTypeName . fmap T.pack $ segments

doParse :: Parser a -> Text -> a
doParse parser text = case runParser parser () "" text of
  Left err  -> error $ show err
  Right val -> val

referencedTypeName :: S.Referenced S.Schema -> TypeName
referencedTypeName (S.Ref ref)       = doParse parseRef $ S.getReference ref
referencedTypeName (S.Inline schema) = error $ show schema

keyedTypeName :: Text -> TypeName
keyedTypeName = doParse parseKey
