{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Web.Styles where

import           Reflex.Dom  ((=:))

import qualified Data.Map    as M
import           Data.Monoid
import           Data.Text   (Text)
import qualified Data.Text   as T

import           Web.Style

width :: Int -> StyleMap
width w = width' $ px w

width' :: Text -> StyleMap
width' w = "width" =: w

height :: Int -> StyleMap
height h = height' $ px h

height' :: Text -> StyleMap
height' h = "height" =: h

minWidth :: Int -> StyleMap
minWidth w = minWidth' $ px w

minWidth' :: Text -> StyleMap
minWidth' w = "min-width" =: w

minHeight :: Int -> StyleMap
minHeight h = minHeight' $ px h

minHeight' :: Text -> StyleMap
minHeight' h = "min-height" =: h

posRel :: StyleMap
posRel = pos "relative"

borderBox :: StyleMap
borderBox = "box-sizing" =: "border-box"

posAbs :: StyleMap
posAbs = pos "absolute"

pos :: Text -> StyleMap
pos p = "position" =: p

posFix :: StyleMap
posFix = pos "fixed"

left :: Int -> StyleMap
left x = left' $ px x

left' :: Text -> StyleMap
left' x = "left" =: x

top :: Int -> StyleMap
top y = top' $ px y

top' :: Text -> StyleMap
top' y = "top" =: y

color :: Text -> StyleMap
color color = "color" =: color

backgroundColor :: Text -> StyleMap
backgroundColor color = "background-color" =: color

border :: Text -> StyleMap
border b = "border" =: b

preserve3d :: StyleMap
preserve3d = "transform-style" =: "preserve-3d"

transform :: Text -> StyleMap
transform t = "transform" =: t

display :: Text -> StyleMap
display d = "display" =: d

displayFlex :: StyleMap
displayFlex = display "flex"

displayInlineFlex :: StyleMap
displayInlineFlex = display "inline-flex"

flexDir :: Text -> StyleMap
flexDir dir = "flex-direction" =: dir

flexRow :: StyleMap
flexRow = flexDir "row"

flexCol :: StyleMap
flexCol = flexDir "column"

displayInlineBlock :: StyleMap
displayInlineBlock = display "inline-block"

alignItems :: Text -> StyleMap
alignItems a = "align-items" =: a

alignItemsCenter :: StyleMap
alignItemsCenter = alignItems "center"

justifyContent :: Text -> StyleMap
justifyContent j = "justify-content" =: j

justifyContentCenter :: StyleMap
justifyContentCenter = justifyContent "center"

textAlignCenter :: StyleMap
textAlignCenter = "text-align" =: "center"

margin :: Int -> StyleMap
margin pixels = marginTRBL pixels pixels pixels pixels

marginVH :: Int -> Int -> StyleMap
marginVH vertical horizontal = marginTRBL vertical horizontal vertical horizontal

marginTRBL :: Int -> Int -> Int -> Int -> StyleMap
marginTRBL top right bottom left =
  marginTRBL' (px top) (px right) (px bottom) (px left)

marginTRBL' :: Text -> Text -> Text -> Text -> StyleMap
marginTRBL' top right bottom left =
  "margin" =: (top <> " " <> right <> " " <> bottom <> " " <> left)

noMargin :: StyleMap
noMargin = "margin" =: "0"

padding :: Int -> StyleMap
padding pixels = paddingTRBL pixels pixels pixels pixels

paddingVH :: Int -> Int -> StyleMap
paddingVH vertical horizontal = paddingTRBL vertical horizontal vertical horizontal

paddingTRBL :: Int -> Int -> Int -> Int -> StyleMap
paddingTRBL top right bottom left =
  paddingTRBL' (px top) (px right) (px bottom) (px left)

paddingTRBL' :: Text -> Text -> Text -> Text -> StyleMap
paddingTRBL' top right bottom left =
  "padding" =: (top <> " " <> right <> " " <> bottom <> " " <> left)

noPadding :: StyleMap
noPadding = "padding" =: "0"

fullWidth :: StyleMap
fullWidth = width' "100%"

fullHeight :: StyleMap
fullHeight = height' "100%"

fullWindow :: StyleMap
fullWindow = fullHeight <> fullWidth

userSelectNone :: StyleMap
userSelectNone = "user-select" =: "none" <> "-webkit-user-select" =: "none"

cursorArrow :: StyleMap
cursorArrow = "cursor" =: "default"
