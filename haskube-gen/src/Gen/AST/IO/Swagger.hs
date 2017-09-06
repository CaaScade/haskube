{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gen.AST.IO.Swagger where

import           Control.Applicative
import           Control.Monad

import           Control.Lens
import           Control.Lens.At
import           Control.Lens.Traversal

import qualified Data.Aeson             as AE
import qualified Data.Aeson.Lens        as AE
import           Data.Aeson.Types       (parseEither)
import qualified Data.ByteString.Lazy   as BS
import           Data.Maybe             (fromMaybe)
import qualified Data.Swagger           as S

parseSwagger :: AE.Value -> Either String S.Swagger
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
    Left err      -> error err
    Right swagger -> return swagger
