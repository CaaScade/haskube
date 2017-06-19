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
import qualified Data.HashMap.Strict.InsOrd as HI
import qualified Data.Swagger           as S
import qualified Data.Text              as T
import           Data.Maybe             (fromJust, fromMaybe, isJust)

import Gen.AST

parseSwagger :: AE.Value -> (Either String S.Swagger)
parseSwagger =
  parseEither AE.parseJSON . setType
  where definitionType = AE.key "definitions" . AE.members . AE._Object . at "type"
        setType :: AE.Value -> AE.Value
        setType = definitionType %~ Just . fromMaybe "object"

readSwagger :: FilePath -> IO S.Swagger
readSwagger file = do
  swaggerText <- BS.readFile file
  let result = do
        swaggerVal <- AE.eitherDecode swaggerText
        parseSwagger swaggerVal
  case result of
    Left err -> error err
    Right swagger -> return swagger

printSome :: (Show a) => a -> IO ()
printSome = putStrLn. take 200 . show

main :: IO ()
main = do
  swag <- readSwagger "swagger.json"
  printSome $ rewriteDefinitions $ S._swaggerDefinitions swag
  print $ HI.lookup "io.k8s.apimachinery.pkg.api.resource.Quantity" $ S._swaggerDefinitions swag
