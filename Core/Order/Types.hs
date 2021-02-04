{-# LANGUAGE TemplateHaskell #-}

module Core.Order.Types where

import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types
import ClassyPrelude (Text)
import Platform.Types (commonJSONDeriveMany)
  
data Order = Order
  { orderNumber :: Integer
  , orderTable :: Integer
  , orderItemsSlug :: [Text]
  } deriving(Eq, Show)
  
data OrderIntent = OrderIntent
  { orderIntentTableNumber :: Integer
  , orderIntentItems :: [Text]
  } deriving(Eq, Show)
  
newtype OrderWrapper o = OrderWrapper { orderWrapperOrder :: o } deriving (Eq, Show)
data OrdersWrapper o = OrdersWrapper { ordersWrapperOrders :: [o], ordersWrapperCount :: Int } deriving (Eq, Show)
data OrderError = OrderNotFound String | OrderInvalid String  
  
$(commonJSONDeriveMany 
  [ ''Order
  , ''OrderIntent
  , ''OrderWrapper
  , ''OrdersWrapper
  , ''OrderError])

instance FromRow Order where
  fromRow = Order 
    <$> field
    <*> field
    <*> (fromPGArray <$> field)
    
instance FromRow OrderIntent where
  fromRow = OrderIntent 
    <$> field
    <*> (fromPGArray <$> field)