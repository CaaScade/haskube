{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Applicative
import           Control.Monad

import           Control.Lens
import           Control.Lens.At
import           Control.Lens.Traversal

import qualified Data.Aeson             as AE
import qualified Data.Aeson.Lens        as AE
import           Data.Aeson.Types       (parseEither)
import qualified Data.ByteString.Lazy   as BS
import qualified Data.HashMap.Lazy      as H
import qualified Data.Swagger           as S
import qualified Data.Text              as T

import           Data.Maybe             (fromJust, fromMaybe, isJust)

swaggerVal :: IO AE.Value
swaggerVal = do
  swaggerText <- BS.readFile "swagger.json"
  return . fromJust $ AE.decode swaggerText

parseSwaggerVal :: AE.Value -> (Either String S.Swagger)
parseSwaggerVal =
  parseEither AE.parseJSON . setType
  where definitionType = AE.key "definitions" . AE.members . AE._Object . at "type"
        setType :: AE.Value -> AE.Value
        setType = definitionType %~ Just . fromMaybe "object"

swaggerVal' :: IO (Either String S.Swagger)
swaggerVal' = do
  swaggerText <- BS.readFile "swagger.json"
  return $ AE.eitherDecode swaggerText

printSwaggerField :: (AE.Value -> AE.Value) -> IO ()
printSwaggerField field = printSome =<< fmap field swaggerVal

printSome :: (Show a) => a -> IO ()
printSome = putStrLn. take 200 . show

main :: IO ()
main = do
  root <- swaggerVal
  printSome $ parseSwaggerVal root

fieldSwagger :: AE.Value -> AE.Value
fieldSwagger (AE.Object root) = fromJust $ H.lookup "swagger" root
fieldSwagger _                = error "root is an object"

fieldInfo :: AE.Value -> AE.Value
fieldInfo (AE.Object root) = fromJust $ H.lookup "info" root
fieldInfo _                = error "root is an object"
