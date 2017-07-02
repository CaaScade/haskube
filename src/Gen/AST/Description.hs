{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Gen.AST.Description where

import           Data.Monoid
import           Data.Text   (Text)

newtype Description = Description { _descriptionText :: Text } deriving (Show)

labelDescription :: Text -> Description -> Description
labelDescription label Description{..} =
  Description $ label <> " " <> _descriptionText

appendDescription' :: Description -> Description -> Description
appendDescription' = dConcatWith' " | "

appendDescription :: Maybe Description -> Maybe Description -> Maybe Description
appendDescription = dConcatWith " | "

dConcatWith' :: Text -> Description -> Description -> Description
dConcatWith' sep (Description d0) (Description d1) =
  Description $ d0 <> sep <> d1

dConcatWith :: Text -> Maybe Description -> Maybe Description -> Maybe Description
dConcatWith _ Nothing Nothing       = Nothing
dConcatWith _ d0 Nothing            = d0
dConcatWith _ Nothing d1            = d1
dConcatWith sep (Just d0) (Just d1) = Just $ dConcatWith' sep d0 d1

wrapDescription :: Description -> Description
wrapDescription Description{..} = Description $ "{ " <> _descriptionText <> " }"

nestDescription :: Maybe Description -> Maybe Description -> Maybe Description
nestDescription outer inner = dConcatWith " " outer $ wrapDescription <$> inner

