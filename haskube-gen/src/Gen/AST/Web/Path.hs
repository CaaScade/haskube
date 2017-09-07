{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Gen.AST.Web.Path where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except

import qualified Data.HashMap.Strict.InsOrd as HI
import           Data.Monoid
import qualified Data.Swagger               as S
import           Data.Text                  (Text, pack)
import           Text.Parsec                (ParseError, runParser)
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Text           (Parser)
import           Text.PrettyPrint           (Doc)
import           Text.Show.Pretty           (pPrint, ppDoc, ppShow)

import           Gen.AST.IO.Swagger

data PathSegment = ConstSegment Text | ParamSegment Text deriving (Show)

-- | Allows empty segments (trailing slash, back-to-back slashes)
parsePlainSegment :: Parser Text
parsePlainSegment = pack <$> many alphaNum

parseBracedSegment :: Parser Text
parseBracedSegment = char '{' *> parsePlainSegment <* char '}'

parseSegment :: Parser PathSegment
parseSegment =
  (ConstSegment <$> parsePlainSegment) <|> (ParamSegment <$> parseBracedSegment)

parsePath :: Parser [PathSegment]
parsePath = char '/' *> (parseSegment `sepBy1` char '/')

doParse :: (MonadError Doc m) => Parser a -> Text -> m a
doParse parser text =
  case runParser parser () "" text of
    Left err  -> throwError . ppDoc $ (text, err)
    Right val -> return val

test :: IO ()
test = do
  swag <- readSwagger "swagger.json"
  let paths_ = fmap pack . HI.keys $ S._swaggerPaths swag
  let paths = mapM (doParse parsePath :: Text -> Either Doc [PathSegment]) paths_
  pPrint paths


