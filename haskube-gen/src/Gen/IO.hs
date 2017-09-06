{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gen.IO where

import           Control.Applicative
import           Control.Monad

import           Language.Haskell.Exts        (Module)
import           Language.Haskell.Exts.Pretty (prettyPrint)

import           Data.Foldable                (foldl')
import           Data.Monoid
import qualified Data.Swagger                 as S
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import           Text.Show.Pretty             (pPrint, ppShow)

import           System.Directory

import           Gen.AST
import           Gen.AST.BuiltIn              (builtInNewtypesModule')
import           Gen.AST.Class
import           Gen.AST.IO.Swagger
import           Gen.AST.Types

readTypes :: FilePath -> IO (Either ASTError [Type])
readTypes file = do
  swag <- readSwagger file
  return . rewriteDefinitions $ S._swaggerDefinitions swag

printSome :: (Show a) => a -> IO ()
printSome = putStrLn. take 200 . show

printError :: ASTError -> IO ()
printError ASTError{..} = do
  T.putStrLn _asteLabel
  pPrint _asteError
  pPrint $ reverse _asteStack

modulePath :: Text -> FilePath
modulePath = (<> ".hs") . T.unpack . T.replace "." "/"

pathDirectory :: FilePath -> FilePath
pathDirectory path = if len > 0 then take len path else "."
  where len = foldl' f 0 $ zip [1..] path
        f lastIndex (index, char) = if char == '/' then index else lastIndex

-- | NOTE: Expects a trailing slash in the prefix.
writeModule_ :: FilePath -> Text -> String -> IO ()
writeModule_ prefix moduleName moduleContents = do
  createDirectoryIfMissing True directory
  writeFile filePath moduleContents
  where filePath = prefix <> modulePath moduleName
        directory = pathDirectory filePath

writeModule :: FilePath -> Text -> Module ann -> IO ()
writeModule prefix moduleName aModule =
  writeModule_ prefix moduleName $ prettyPrint aModule

writeBuiltInModule :: FilePath -> IO ()
writeBuiltInModule prefix = do
  contents <- readFile "template/built-ins.hs"
  writeModule_ prefix builtInNewtypesModule' contents

removeDirectoryIfExists :: FilePath -> IO ()
removeDirectoryIfExists directory = do
  exists <- doesDirectoryExist directory
  when exists $ removeDirectoryRecursive directory

printModules :: [Text] -> IO ()
printModules = mapM_ $ putStrLn . ("                     , " <> ) . T.unpack
