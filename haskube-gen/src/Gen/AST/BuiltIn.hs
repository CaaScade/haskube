{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Gen.AST.BuiltIn where

import qualified Data.Swagger  as S
import           Data.Text     (Text)

import           Gen.AST.Class
import           Gen.AST.Types

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
nullName = SimpleName builtInNewtypesModule "Null"

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
