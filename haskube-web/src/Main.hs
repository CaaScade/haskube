{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main where

import           Reflex.Dom

import           Data.Map      (Map)
import qualified Data.Map      as M
import           Data.Monoid
import           Data.Text     (Text)
import qualified Data.Text     as T

import           Backend.Types
import           Web.Layout
import qualified Web.Solarized as S
import           Web.Style
import qualified Web.Styles    as S

main :: IO ()
main = mainWidgetWithHead headEl $ do
  splashEl

css :: Text
css = toCssText $ body <> html
  where body = "body" =: (S.fullWindow <> S.noMargin <> fontFamily)
        html = "html" =: S.fullWindow

fontFamily :: StyleMap
fontFamily =
  "font-family" =:
  "\"Helvetica Neue Light\",\"Helvetica Neue\",Helvetica,Calibri,Candara,Arial,sans-serif"

fontSize :: Int -> StyleMap
fontSize x = fontSize' $ px x

fontSize' :: Text -> StyleMap
fontSize' size = "font-size" =: size

headEl :: (MonadWidget t m) => m ()
headEl = do
  el "style" $ text css

splashEl :: (MonadWidget t m) => m ()
splashEl =
  centeringDiv $ do
    divAttr (toAttr $ S.width 400) $ do
      divAttr
        (toAttr $
         fontSize 38 <> S.color S.base01 <> S.textAlignCenter <>
         S.backgroundColor S.base2 <>
         S.paddingVH 40 0) $ do text "Swag"
      divAttr
        (toAttr $
         fontSize 14 <> S.color S.base00 <> S.backgroundColor S.base3 <>
         S.padding 20) $ do
        text
          "o hello my name is oh what is that you have is it tasty food potatoes are wow"
