{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Core.Item.Types where

import ClassyPrelude (Text)
import Database.PostgreSQL.Simple.FromRow
import Platform.Types

data Item = Item
  { itemSlug :: Text
  , itemName :: Text
  , itemDescription :: Text
  , itemCategory :: Text
  , itemPrice :: Double
  , itemImage :: Maybe Text
  } deriving(Eq, Show)

data ItemIntent = ItemIntent
  { itemIntentName :: Text
  , itemIntentDescription :: Text
  , itemIntentCategory :: Text
  , itemIntentPrice :: String
  , itemIntentImage :: Maybe Text
  } deriving(Eq, Show)
    
data ItemFilter = ItemFilter
  { itemNameFilter :: Maybe Text
  , itemDescriptionFilter :: Maybe Text
  , itemCategoryFilter :: Maybe Text
  } deriving (Eq, Show)

data ItemError = ItemErrorNotFound Text | ItemErrorNotAllowed Text | ItemErrorBadJSON String | ItemErrorCategoryNotFound Text deriving (Eq, Show)
data ItemsWrapper i = ItemsWrapper { itemsWrapperItems :: [i], itemsWrapperItemsCount :: Int } deriving (Eq, Show)
newtype ItemWrapper i = ItemWrapper { itemWrapperItem :: i } deriving (Eq, Show)

-- * Instances

$(commonJSONDeriveMany
  [ ''Item
  , ''ItemIntent
  , ''ItemFilter
  , ''ItemsWrapper
  , ''ItemWrapper
  , ''ItemError])

instance FromRow Item where
  fromRow = Item 
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field