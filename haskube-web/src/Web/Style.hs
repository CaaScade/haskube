{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Web.Style where

import           Reflex.Dom

import qualified Data.Map    as M
import           Data.Monoid
import           Data.Text   (Text)
import qualified Data.Text   as T

type StyleMap = M.Map Text Text

toStyleText :: StyleMap -> Text
toStyleText styleMap = mconcat $ fmap f (M.toList styleMap)
  where f (key, val) = key <> ":" <> val <> ";"

insertStyles :: StyleMap -> M.Map Text Text -> M.Map Text Text
insertStyles styles attrs = M.insert "style" (toStyleText styles) attrs

toAttr :: StyleMap -> M.Map Text Text
toAttr styles = "style" =: (toStyleText styles)

styleIf :: Bool -> StyleMap -> StyleMap
styleIf False _ = mempty
styleIf True x  = x

px :: Int -> Text
px x = (T.pack . show) x <> "px"

type CssMap = M.Map Text StyleMap

toCssText :: CssMap -> Text
toCssText cssMap = mconcat $ fmap f (M.toList cssMap)
  where f (key, val) = key <> " {\n" <> toStyleText val <> "\n}\n"

