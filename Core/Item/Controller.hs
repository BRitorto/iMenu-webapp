{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

module Core.Item.Controller
      ( routes
      , Service(..) 
      ) where

import ClassyPrelude hiding (delete)
import Core.Item.Types
import Web.Slug (Slug)
import Web.Scotty.Internal.Types (ScottyT, ScottyError, ActionT)
import Web.Scotty.Trans
import Web.Scotty (ScottyM)
import qualified Text.Digestive.Form as DF
import Platform.JSONUtil
import Text.Digestive.Form ((.:))
import Network.HTTP.Types
import Data.Aeson (eitherDecode)

class Monad m => Service m where
  getItems :: m [Item]
  getItem :: Text -> m (Either ItemError Item)
  createItem :: ItemIntent -> m (Either ItemError Item)

routes :: (Service m, MonadIO m) => ScottyT LText m ()
routes = do 
  
  get "/api/items" $ do
    result <- lift getItems
    json $ ItemsWrapper result (ClassyPrelude.length result)
    
  post "/api/items" $ do
    req <- body
    let parsedBody = (eitherDecode req :: Either String ItemIntent)
    case parsedBody of
      Left e -> do 
        status badRequest400
        json (ItemErrorBadJSON e)
      Right i -> do
        result <- stopIfError itemErrorHandler $ createItem i
        json $ ItemWrapper result

mayParam :: (ScottyError e, Monad m) => LText -> ActionT e m (Maybe Text)
mayParam name = (Just <$> param name) `rescue` const (return Nothing)

parseItemFilter :: (ScottyError e, Monad m) => ActionT e m ItemFilter
parseItemFilter = ItemFilter <$> mayParam "name" <*> mayParam "description" <*> mayParam "category"

--- * Errors

itemErrorHandler :: (ScottyError e, Monad m) => ItemError -> ActionT e m ()
itemErrorHandler err = case err of
  ItemErrorNotFound _ -> do
    status status404
    json err
  ItemErrorNotAllowed _ -> do
    status status403
    json err

--- * Forms

createItemForm :: (Monad m) => DF.Form [Text] m ItemIntent
createItemForm = ItemIntent <$> "name" .: DF.text Nothing
                            <*> "description" .: DF.text Nothing
                            <*> "category" .: DF.text Nothing
                            <*> "price" .: DF.string Nothing
                            <*> "image" .: DF.optionalText Nothing