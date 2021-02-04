{-# LANGUAGE OverloadedStrings #-}

module Core.Order.Service where

import Core.Order.Types 
import Core.Item.Service 
import ClassyPrelude (Text)
import Control.Monad.Except
  
createOrder :: (OrderRepo m, ItemRepo m) => [Text] -> Integer -> m (Either OrderError Order)
createOrder items table = runExceptT $ do
  --ExceptT $ validateItemsExist items
  lift $ addOrder (OrderIntent table items)
  ExceptT $ getOrder table
  
validateItemsExist :: (OrderRepo m, ItemRepo m) => [Text] -> m (Either OrderError ())
validateItemsExist items = runExceptT $ do
  result <- lift $ itemsExist items
  case result of
    Just False -> throwError $ OrderInvalid "Invalid order"
    _ -> return ()

getOrder :: (OrderRepo m) => Integer -> m (Either OrderError Order)
getOrder table = runExceptT $ do 
  result <- lift $ findOrderByTable table
  case result of
    [order] -> return order
    _ -> throwError $ OrderNotFound "Order not found for table"
    
getOrders :: (OrderRepo m) => m [Order]
getOrders = findOrders
  
finishOrder :: (OrderRepo m) => Integer -> m (Either OrderError ())
finishOrder table = runExceptT $ do
  lift $ deleteOrderByTable table
  
class (Monad m) => OrderRepo m where
    addOrder :: OrderIntent -> m ()
    findOrderByTable :: Integer -> m [Order]
    deleteOrderByTable :: Integer -> m ()
    findOrders :: m [Order]