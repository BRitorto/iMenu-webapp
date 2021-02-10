{-# LANGUAGE OverloadedStrings #-}

module Core.Order.Controller
      ( routes 
      , adminRoutes
      , Service(..) 
      ) where

import ClassyPrelude (LText, length, Text, lift, MonadIO, asText)
import Web.Scotty.Internal.Types (ScottyT)
import Web.Scotty.Trans
import Core.Order.Types
import Platform.JSONUtil (stopIfError)
import Network.HTTP.Types.Status (status404, status400, badRequest400)
import Data.Aeson (eitherDecode)

routes :: (Service m, MonadIO m) => ScottyT LText m ()
routes = do 
        
  post "/api/orders" $ do
     req <- body
     let parsedBody = (eitherDecode req :: Either String OrderIntent)
     case parsedBody of
       Left e -> do 
         status badRequest400
         json (OrderInvalid e)
       Right o -> do
         result <- stopIfError orderErrorHandler $ createOrder (orderIntentItems o) (orderIntentTableNumber o)
         json $ OrderWrapper result

adminRoutes :: (Service m, MonadIO m) => ScottyT LText m ()
adminRoutes = do

  get "/admin/orders" $ do
    result <- lift getOrders
    json $ OrdersWrapper result (ClassyPrelude.length result)

  get "/admin/orders/:table" $ do
    table <- param "table"
    result <- stopIfError orderErrorHandler $ getOrder table
    json $ OrderWrapper result
  
  delete "/admin/orders/:table" $ do
      table <- param "table"
      stopIfError orderErrorHandler $ finishOrder table
      json $ asText "Order finished"

orderErrorHandler :: (ScottyError e, Monad m) => OrderError -> ActionT e m ()
orderErrorHandler err = case err of
  OrderNotFound _ -> do
    status status404
    json err
  OrderInvalid _ -> do
    status status400
    json err 

class Monad m => Service m where
  getOrder :: Integer -> m (Either OrderError Order)
  getOrders :: m [Order]
  createOrder :: [Text] -> Integer -> m (Either OrderError Order)
  finishOrder :: Integer -> m (Either OrderError ())
