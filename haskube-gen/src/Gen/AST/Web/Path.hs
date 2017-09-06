{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gen.AST.Web.Path where

import           Control.Applicative
import           Control.Monad

import qualified Data.HashMap.Strict.InsOrd as HI
import qualified Data.Swagger        as S
import           Text.Show.Pretty       (pPrint, ppShow)

import Gen.AST.IO.Swagger -- debug

test :: IO ()
test = do
  swag <- readSwagger "swagger.json"
  let paths = S._swaggerPaths swag
  pPrint . HI.keys $ paths

