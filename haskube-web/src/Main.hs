{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main where

import           Reflex.Dom    hiding (button)

import           Control.Monad

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

button :: (MonadWidget t m) => Text -> StyleMap -> m (Event t ())
button label style = do
  (e, _) <- divStyle' (S.cursorArrow <> S.userSelectNone <> style) $ text label
  return $ domEvent Click e

charButton :: (MonadWidget t m) => Text -> m (Event t ())
charButton char =
  button
    char
    (fontWeightBold <> fontSize 20 <> S.displayInlineBlock <> S.textAlignCenter <>
     S.width 20 <>
     S.height 20 <>
     S.color "white" <>
     S.backgroundColor "darkgrey" <>
     "line-height" =:
     "20px")

commitButton :: (MonadWidget t m) => m (Event t ())
commitButton =
  button
    "🡸 Commit"
    (fontSize 20 <> fontWeightMedium <> S.backgroundColor "green" <>
     S.color "white" <>
     S.marginTRBL 10 0 0 0 <>
     S.paddingVH 5 10)

plusButton :: (MonadWidget t m) => m (Event t ())
plusButton = charButton "+"

minusButton :: (MonadWidget t m) => m (Event t ())
minusButton = charButton "-"

editPostgresCell :: (MonadWidget t m) => m ()
editPostgresCell =
  divStyle mempty $ do
    divStyle
      (fontSize 24 <> fontWeightBold <> S.paddingTRBL 10 10 5 10 <>
       S.color "white" <>
       S.backgroundColor "blue") $ do text "PostgreSQL"
    divStyle
      (fontSize 18 <> fontWeightMedium <> S.paddingTRBL 0 10 10 10 <>
       S.color "black" <>
       S.backgroundColor "white") $ do
      divStyle (S.paddingTRBL 10 0 0 0) $ do
        text "masters "
        void $ minusButton
        text " 1 "
        void $ plusButton
      divStyle (S.paddingTRBL 10 0 0 0) $ do
        text "slave "
        void $ minusButton
        text " 1 "
        void $ plusButton

postgresCell :: (MonadWidget t m) => m ()
postgresCell =
  divStyle mempty $ do
    divStyle
      (fontSize 24 <> fontWeightBold <> S.paddingTRBL 10 10 5 10 <>
       S.color "white" <>
       S.backgroundColor "blue") $ do text "PostgreSQL"
    divStyle
      (fontSize 18 <> fontWeightMedium <> S.paddingTRBL 0 10 10 10 <>
       S.color "black" <>
       S.backgroundColor "white") $ do
      divStyle (S.paddingTRBL 10 0 0 0) $ do text "masters 1"
      divStyle (S.paddingTRBL 10 0 0 0) $ do text "slaves 1"

currentCell :: (MonadWidget t m) => m ()
currentCell =
  divStyle (S.displayFlex <> S.flexCol) $ do
    divStyle
      (fontSize 20 <> fontWeightMedium <> S.paddingTRBL 10 10 5 20 <>
       S.color "white" <>
       S.backgroundColor "grey") $ do text "Running"
    divStyle (S.backgroundColor "grey" <> S.paddingTRBL 0 10 10 10) $ do
      postgresCell

editingCell :: (MonadWidget t m) => m ()
editingCell =
  divStyle (S.displayFlex <> S.flexCol) $ do
    divStyle
      (fontSize 20 <> fontWeightMedium <> S.paddingTRBL 10 10 5 20 <>
       S.color "white" <>
       S.backgroundColor "grey") $ do text "Editing"
    divStyle (S.backgroundColor "grey" <> S.paddingTRBL 0 10 10 10) $ do
      editPostgresCell
      void $ commitButton

historyGrid :: (MonadWidget t m) => m ()
historyGrid =
  divStyle (S.displayFlex <> S.flexRow <> S.backgroundColor "grey") $ do
    currentCell
    divStyle (S.width 4 <> S.backgroundColor "white") $ return ()
    editingCell
