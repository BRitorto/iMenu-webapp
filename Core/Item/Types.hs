{-# LANGUAGE TemplateHaskell #-}

module Core.Item.Types where

import ClassyPrelude (Text)
import Database.PostgreSQL.Simple.FromRow
import Platform.JSONUtil
import Web.Slug (Slug)

data Item = Item
  { itemSlug :: Text
  , itemName :: Text
  , itemDescription :: Text
  , itemCategory :: Text
  , itemPrice :: Double
  , itemImage :: Text
  } deriving(Eq, Show)
  
data ItemFilter = ItemFilter
  { itemNameFilter :: Maybe Text
  , itemDescriptionFilter :: Maybe Text
  , itemCategoryFilter :: Maybe Text
  } deriving (Eq, Show)

newtype ItemError = ItemErrorNotFound Text deriving (Eq, Show)
data ItemsWrapper i = ItemsWrapper { itemsWrapperItems :: [i], itemsWrapperItemsCount :: Int } deriving (Eq, Show)

-- * Instances

$(commonJSONDeriveMany
  [ ''Item
  , ''ItemFilter
  , ''ItemsWrapper])

instance FromRow Item where
  fromRow = Item 
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field