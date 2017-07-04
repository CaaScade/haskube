{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gen.IO where

import           Control.Applicative
import           Control.Monad

import           Control.Lens
import           Control.Lens.At
import           Control.Lens.Traversal

import           Language.Haskell.Exts        (Module)
import           Language.Haskell.Exts.Pretty (prettyPrint)

import qualified Data.Aeson                   as AE
import qualified Data.Aeson.Lens              as AE
import           Data.Aeson.Types             (parseEither)
import qualified Data.ByteString.Lazy         as BS
import           Data.Foldable                (foldl')
import qualified Data.HashMap.Strict.InsOrd   as HI
import           Data.Maybe                   (fromJust, fromMaybe, isJust)
import           Data.Monoid
import qualified Data.Swagger                 as S
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import           Text.Show.Pretty             (pPrint, ppShow)

import           System.Directory

import           Gen.AST
import           Gen.AST.Class
import           Gen.AST.Types

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
writeModule :: FilePath -> Text -> Module ann -> IO ()
writeModule prefix moduleName aModule = do
  createDirectoryIfMissing True directory
  writeFile filePath $ prettyPrint aModule
  where filePath = prefix <> modulePath moduleName
        directory = pathDirectory filePath

removeDirectoryIfExists :: FilePath -> IO ()
removeDirectoryIfExists directory = do
  exists <- doesDirectoryExist directory
  when exists $ removeDirectoryRecursive directory
