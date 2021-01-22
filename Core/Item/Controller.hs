{-# LANGUAGE OverloadedStrings #-}

module Core.Item.Controller 
      ( routes
      , Service(..) 
      ) where

import ClassyPrelude hiding (delete)
import Core.Item.Types
import Web.Slug (Slug)
import Web.Scotty.Internal.Types (ScottyT, ScottyError, ActionT)
import Web.Scotty.Trans

class Monad m => Service m where
  getItems :: ItemFilter -> m [Item]
  getItem :: Slug -> m (Either ItemError Item)
  createItem :: Item -> m (Either ItemError Item)

routes :: (Service m, MonadIO m) => ScottyT LText m ()
routes = do

  get "/api/items" $ do
    itemFilter <- parseItemFilter
    result <- lift $ getItems itemFilter
    json $ ItemsWrapper result (ClassyPrelude.length result)
    
mayParam :: (ScottyError e, Monad m) => LText -> ActionT e m (Maybe Text)
mayParam name = (Just <$> param name) `rescue` const (return Nothing)    

parseItemFilter :: (ScottyError e, Monad m) => ActionT e m ItemFilter
parseItemFilter = ItemFilter <$> mayParam "name" <*> mayParam "description"