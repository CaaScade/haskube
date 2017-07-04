{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Applicative
import           Control.Monad

import           Control.Lens
import           Control.Lens.At
import           Control.Lens.Traversal

import           Language.Haskell.Exts.Pretty

import qualified Data.Aeson                   as AE
import qualified Data.ByteString.Lazy         as BS
import qualified Data.HashMap.Strict          as H
import           Data.Monoid
import qualified Data.Swagger                 as S
import           Data.Text                    (unpack)
import           Text.Show.Pretty             (pPrint, ppShow)

import           Gen.AST
import           Gen.AST.Class
import           Gen.AST.Code
import           Gen.IO

main :: IO ()
main = do
  swag <- readSwagger "swagger.json"
  writeFile "swag.txt" $ ppShow $ S._swaggerDefinitions swag
  case rewriteDefinitions $ S._swaggerDefinitions swag of
    Left error  -> printError error
    Right types -> do
      writeFile "haskube.txt" $ ppShow types
      BS.writeFile "haskube.json" $ AE.encode types
      let modules = mkModules types
          f (key, val) = writeFile ("output/" <> unpack key <> ".hs") $ prettyPrint val
      mapM_ f $ H.toList modules
