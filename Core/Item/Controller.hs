{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

module Core.Item.Controller
      ( routes
      , adminRoutes
      , Service(..) 
      ) where

import ClassyPrelude hiding (delete)
import Core.Item.Types
import Web.Scotty.Internal.Types (ScottyT, ScottyError, ActionT)
import Web.Scotty.Trans
import qualified Text.Digestive.Form as DF
import Platform.JSONUtil
import Text.Digestive.Form ((.:))
import Network.HTTP.Types
import Data.Aeson (eitherDecode)

class Monad m => Service m where
  getItems :: m [Item]
  getItem :: Text -> m (Either ItemError Item)
  createItem :: ItemIntent -> m (Either ItemError Item)
  getItemsByCategory :: Text -> m (Either ItemError [Item])
  deleteItem :: Text -> m (Either ItemError ())
  updateItem :: Text -> ItemIntent -> m (Either ItemError Item)

routes :: (Service m, MonadIO m) => ScottyT LText m ()
routes = do 
  
  get "/api/items" $ do
    result <- lift getItems
    json $ ItemsWrapper result (ClassyPrelude.length result)
    
  get "/api/items/:category" $ do
      category <- param "category"
      result <- stopIfError itemErrorHandler $ getItemsByCategory category
      json $ ItemsWrapper result (ClassyPrelude.length result)
 
adminRoutes :: (Service m, MonadIO m) => ScottyT LText m ()
adminRoutes = do  
      
  delete "/admin/items/:slug" $ do
      slug <- param "slug"
      stopIfError itemErrorHandler $ deleteItem slug
      json $ asText "Item deleted"

  post "/admin/items" $ do
      req <- body
      let parsedBody = (eitherDecode req :: Either String ItemIntent)
      case parsedBody of
        Left e -> do 
          status badRequest400
          json (ItemErrorBadJSON e)
        Right i -> do
          result <- stopIfError itemErrorHandler $ createItem i
          json $ ItemWrapper result
  
  put "/admin/items/:slug" $ do
      req <- body
      slug <- param "slug"
      let parsedBody = (eitherDecode req :: Either String ItemIntent)
      case parsedBody of
        Left e -> do 
          status badRequest400
          json (ItemErrorBadJSON e)
        Right i -> do
          result <- stopIfError itemErrorHandler $ updateItem slug i
          json $ ItemWrapper result
 

--- * Errors

itemErrorHandler :: (ScottyError e, Monad m) => ItemError -> ActionT e m ()
itemErrorHandler err = case err of
  ItemErrorNotFound _ -> do
    status status404
    json err
  ItemErrorNotAllowed _ -> do
    status status403
    json err
  ItemErrorBadJSON _ -> do
    status status400
    json err
  ItemErrorCategoryNotFound _ -> do
    status status404
    json err

--- * Forms

createItemForm :: (Monad m) => DF.Form [Text] m ItemIntent
createItemForm = ItemIntent <$> "name" .: DF.text Nothing
                            <*> "description" .: DF.text Nothing
                            <*> "category" .: DF.text Nothing
                            <*> "price" .: DF.string Nothing
                            <*> "image" .: DF.optionalText Nothing