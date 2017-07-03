{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Applicative
import           Control.Monad

import           Control.Lens
import           Control.Lens.At
import           Control.Lens.Traversal

import qualified Data.Aeson                 as AE
import qualified Data.ByteString.Lazy       as BS
import qualified Data.Swagger               as S
import           Text.Show.Pretty           (pPrint, ppShow)

import           Gen.AST
import           Gen.AST.Class
import           Gen.IO

main :: IO ()
main = do
  swag <- readSwagger "swagger.json"
  writeFile "swag.txt" $ ppShow $ S._swaggerDefinitions swag
  case fmap toModules . rewriteDefinitions $ S._swaggerDefinitions swag of
    Left error  -> printError error
    Right modules -> do
      writeFile "haskube.txt" $ ppShow modules
      BS.writeFile "haskube.json" $ AE.encode modules
