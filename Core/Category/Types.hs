{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Core.Category.Types where

import ClassyPrelude (Text)
import Platform.Types
import Database.PostgreSQL.Simple.FromRow

data CategoryError = CategoryDuplicated Text | CategoryNotFound Text | CategoryErrorBadJSON String deriving (Eq, Show)
newtype Category = Category { categoryName :: Text } deriving (Eq, Show) 
newtype CategoryWrapper c = CategoryWrapper { categoryWrapperCategory :: c } deriving (Eq, Show)
data CategoriesWrapper i = CategoriesWrapper { categoriesWrapperCategories :: [i], categoriesWrapperCount :: Int } deriving (Eq, Show)

$(commonJSONDeriveMany
  [ ''Category
  , ''CategoryError
  , ''CategoryWrapper
  , ''CategoriesWrapper])
  
instance FromRow Category where
  fromRow = Category 
    <$> field