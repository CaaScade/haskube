{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Lib where

import           Control.Monad.Except

import           Data.Aeson           (FromJSON)
import           Data.Monoid
import           Data.Text            (Text, pack, unpack)

import           Network.Wreq

class ToQueryParam a where
  toQueryParam :: a -> [Text]

instance ToQueryParam () where
  toQueryParam () = []

instance ToQueryParam Text where
  toQueryParam = pure

instance ToQueryParam Bool where
  toQueryParam True  = pure "true"
  toQueryParam False = pure "false"

instance ToQueryParam Int where
  toQueryParam = pure . pack . show

class Request a where
  setQueryParams :: a -> Options -> Options
  -- | Returns the path with params filled in.
  requestPath :: a -> Text

class SimpleResponse request where
  type SimpleResponseType request :: *

getRequestWith
  :: (Request req, SimpleResponse req, FromJSON (SimpleResponseType req))
  => Text -> Options -> req -> ExceptT JSONError IO (Response (SimpleResponseType req))
getRequestWith server options_ req = do
  response <- liftIO $ getWith options $ unpack $ server <> path
  asJSON response
  where
    options = setQueryParams req options_
    path = requestPath req
