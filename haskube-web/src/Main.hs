{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Reflex.Dom

import           Data.Map    (Map)
import qualified Data.Map    as M
import           Data.Monoid
import           Data.Text   (Text)
import qualified Data.Text   as T

main :: IO ()
main = mainWidgetWithHead headEl $ display =<< count =<< button "ClickMe"

css :: Text
css = toCssText $ body <> html
  where body = "body" =: (fullWindow <> noMargin)
        html = "html" =: fullWindow

headEl :: (MonadWidget t m) => m ()
headEl = do
  el "style" $ text css

type StyleMap = Map Text Text

toStyleText :: StyleMap -> Text
toStyleText styleMap = mconcat $ fmap f (M.toList styleMap)
  where f (key, val) = key <> ":" <> val <> ";"

insertStyles :: StyleMap -> Map Text Text -> Map Text Text
insertStyles styles attrs = M.insert "style" (toStyleText styles) attrs

toAttr :: StyleMap -> Map Text Text
toAttr styles = "style" =: (toStyleText styles)

styleIf :: Bool -> StyleMap -> StyleMap
styleIf False _ = mempty
styleIf True x = x

px :: Int -> Text
px x = (T.pack . show) x <> "px"

type CssMap = Map Text StyleMap

toCssText :: CssMap -> Text
toCssText cssMap = mconcat $ fmap f (M.toList cssMap)
  where f (key, val) = key <> " {\n" <> toStyleText val <> "\n}\n"

noMargin :: StyleMap
noMargin = "margin" =: "0"

noPadding :: StyleMap
noPadding = "padding" =: "0"

fullWidth :: StyleMap
fullWidth = width' "100%"

fullHeight :: StyleMap
fullHeight = height' "100%"

fullWindow :: StyleMap
fullWindow = fullHeight <> fullWidth

width :: Int -> StyleMap
width w = width' $ px w

width' :: Text -> StyleMap
width' w = "width" =: w

height :: Int -> StyleMap
height h = height' $ px h

height' :: Text -> StyleMap
height' h = "height" =: h
