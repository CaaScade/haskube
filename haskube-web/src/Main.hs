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
import           Web.Style
import qualified Web.Styles    as S

main :: IO ()
main = mainWidgetWithHead headEl $ display =<< count =<< button "ClickMe"

css :: Text
css = toCssText $ body <> html
  where body = "body" =: (S.fullWindow <> S.noMargin)
        html = "html" =: S.fullWindow

headEl :: (MonadWidget t m) => m ()
headEl = do
  el "style" $ text css
