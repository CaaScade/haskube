{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module KubeAuth where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Network.Wreq as W

import qualified KubeConfig as K

getAuthHeader :: K.AuthContext -> W.Auth
getAuthHeader config = W.basicAuth username password
  where username = T.encodeUtf8 $ K._upUsername userPassword
        password = T.encodeUtf8 $ K._upPassword userPassword
        userPassword = K._authUserPassword config
