{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Web.Layout where

import           Reflex.Dom

import qualified Data.Map    as M
import           Data.Monoid
import           Data.Text   (Text)

import           Web.Style
import qualified Web.Styles  as S

overlay :: (MonadWidget t m) => m a -> m a
overlay contents =
  elAttr "div" (toAttr $ S.displayFlex
               <> S.fullWindow
               <> S.justifyContent "center"
               <> S.alignItems "center"
               <> S.posFix
               <> S.top 0) contents

centeringDiv :: (MonadWidget t m) => m a -> m a
centeringDiv content =
  divAttr (toAttr $ S.fullWindow
                <> S.displayFlex
                <> S.flexCol
                <> "justify-content" =: "center"
                <> "align-items" =: "center"
               ) content

divAttr :: (MonadWidget t m) => M.Map Text Text -> m a -> m a
divAttr = elAttr "div"
divAttr' :: (MonadWidget t m) => M.Map Text Text -> m a -> m (El t, a)
divAttr' = elAttr' "div"

rowDiv :: (MonadWidget t m) => m a -> m a
rowDiv content =
  divAttr (toAttr $ S.displayFlex
               <> S.flexRow) content

columnDiv :: (MonadWidget t m) => m a -> m a
columnDiv content =
  divAttr (toAttr $ S.displayFlex
               <> S.flexCol) content
