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
main = mainWidgetWithHead headEl $ do
  centeringDiv $ do
    historyGrid

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

fontWeightBold :: StyleMap
fontWeightBold = "font-weight" =: "bold"

fontWeightMedium :: StyleMap
fontWeightMedium = "font-weight" =: "medium"

fontWeightNormal :: StyleMap
fontWeightNormal = "font-weight" =: "normal"

headEl :: (MonadWidget t m) => m ()
headEl = do
  el "style" $ text css

{-
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
-}

editPostgresCell :: (MonadWidget t m) => m ()
editPostgresCell =
  divAttr (toAttr mempty) $ do
    divAttr
      (toAttr $
       fontSize 24 <> fontWeightBold <> S.paddingTRBL 10 10 5 10 <>
       S.color "white" <>
       S.backgroundColor "blue") $ do text "PostgreSQL"
    divAttr
      (toAttr $
       fontSize 18 <> fontWeightMedium <> S.paddingTRBL 0 10 10 10 <>
       S.color "black" <>
       S.backgroundColor "white") $ do
      divAttr (toAttr $ S.paddingTRBL 10 0 0 0) $ do text "masters 1 -/+"
      divAttr (toAttr $ S.paddingTRBL 10 0 0 0) $ do text "slaves 1 -/+"

postgresCell :: (MonadWidget t m) => m ()
postgresCell =
  divAttr (toAttr mempty) $ do
    divAttr
      (toAttr $
       fontSize 24 <> fontWeightBold <> S.paddingTRBL 10 10 5 10 <>
       S.color "white" <>
       S.backgroundColor "blue") $ do text "PostgreSQL"
    divAttr
      (toAttr $
       fontSize 18 <> fontWeightMedium <> S.paddingTRBL 0 10 10 10 <>
       S.color "black" <>
       S.backgroundColor "white") $ do
      divAttr (toAttr $ S.paddingTRBL 10 0 0 0) $ do text "masters 1"
      divAttr (toAttr $ S.paddingTRBL 10 0 0 0) $ do text "slaves 1"

currentCell :: (MonadWidget t m) => m ()
currentCell =
  columnDiv $ do
    divAttr
      (toAttr $
       fontSize 20 <> fontWeightMedium <> S.paddingTRBL 10 10 5 20 <>
       S.color "white" <>
       S.backgroundColor "grey") $ do text "Running"
    divAttr (toAttr $ S.backgroundColor "grey" <> S.paddingTRBL 0 10 10 10) $ do
      postgresCell

commitButton :: (MonadWidget t m) => m ()
commitButton =
  divAttr (toAttr $ fontSize 20 <> fontWeightMedium <> S.backgroundColor "green" <> S.color "white" <> S.marginTRBL 10 0 0 0 <> S.paddingVH 5 10) $ text "<- Commit"

editingCell :: (MonadWidget t m) => m ()
editingCell =
  columnDiv $ do
    divAttr
      (toAttr $
       fontSize 20 <> fontWeightMedium <> S.paddingTRBL 10 10 5 20 <>
       S.color "white" <>
       S.backgroundColor "grey") $ do text "Editing"
    divAttr (toAttr $ S.backgroundColor "grey" <> S.paddingTRBL 0 10 10 10) $ do
      editPostgresCell
      commitButton

historyGrid :: (MonadWidget t m) => m ()
historyGrid =
  rowDiv $ do
    currentCell
    divAttr (toAttr $ S.width 4 <> S.backgroundColor "white") $ return ()
    editingCell
