{-# LANGUAGE OverloadedStrings #-}

module Core.Category.Controller 
      ( routes 
      , adminRoutes
      , Service(..) 
      ) where

import ClassyPrelude
import Core.Category.Types
import Web.Scotty.Internal.Types (ScottyT)
import Web.Scotty.Trans
import Data.Aeson (eitherDecode)
import Network.HTTP.Types.Status (badRequest400, status404, status400, status409)
import Platform.JSONUtil

routes :: (Service m, MonadIO m) => ScottyT LText m ()
routes =  do
  
  get "/api/categories" $ do
    result <- lift getCategories
    json $ CategoriesWrapper result (ClassyPrelude.length result)

adminRoutes :: (Service m, MonadIO m) => ScottyT LText m ()
adminRoutes =  do
  
  post "/admin/categories" $ do
     req <- body
     let parsedBody = (eitherDecode req :: Either String Category)
     case parsedBody of
       Left e -> do 
         status badRequest400
         json (CategoryErrorBadJSON e)
       Right c -> do
         result <- stopIfError categoryErrorHandler $ createCategory (categoryName c)
         json $ CategoryWrapper result

--- * Errors

categoryErrorHandler :: (ScottyError e, Monad m) => CategoryError -> ActionT e m ()
categoryErrorHandler err = case err of
  CategoryDuplicated _ -> do
    status status409
    json err
  CategoryNotFound _ -> do
    status status404
    json err
  CategoryErrorBadJSON _ -> do
    status status400
    json err 

class Monad m => Service m where
  createCategory :: Text -> m (Either CategoryError Category)
  getCategories :: m [Category]