{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
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
  elAttr
    "div"
    (S.displayFlex <> S.fullWindow <> S.justifyContent "center" <>
     S.alignItems "center" <>
     S.posFix <>
     S.top 0)
    contents

centeringDiv :: (MonadWidget t m) => m a -> m a
centeringDiv content =
  divStyle
    (S.fullWindow <> S.displayFlex <> S.flexCol <> "justify-content" =: "center" <>
     "align-items" =:
     "center")
    content

div' :: (MonadWidget t m) => M.Map Text Text -> StyleMap -> m a -> m (El t, a)
div' attrs styles = elAttr' "div" (attrs <> toAttr styles)

divStyle :: (MonadWidget t m) => StyleMap -> m a -> m a
divStyle = elAttr "div" . toAttr
divStyle' :: (MonadWidget t m) => StyleMap -> m a -> m (El t, a)
divStyle' = elAttr' "div" . toAttr

buttonStyle :: (MonadWidget t m) => Text -> StyleMap -> m (Event t ())
buttonStyle label style = do
  (e, _) <- elAttr' "button" (toAttr style) $ text label
  return $ domEvent Click e

{-
data Div m a = Div
  { _tagStyle    :: StyleMap
  , _tagAttr     :: M.Map Text Text
  , _tagContents :: m a
  }

elDiv :: (MonadWidget t m) => Div m a -> m a
elDiv Div{..} = elAttr "div" (_tagAttr <> toAttr _tagStyle) _tagContents

mkRow :: (MonadWidget t m) => [Div m ()] -> Div m ()
mkRow cols =
  Div (S.displayFlex <> S.flexRow) mempty $ mapM_ elDiv cols

mkCol :: (MonadWidget t m) => [Div m ()] -> Div m ()
mkCol rows =
  Div (S.displayFlex <> S.flexCol) mempty $ mapM_ elDiv rows
-}
