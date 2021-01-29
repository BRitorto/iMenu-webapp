{-# LANGUAGE OverloadedStrings #-}

module Core.Item.Service where
  
import Core.Item.Types
import Web.Slug
import ClassyPrelude
import Control.Monad.Except (runExceptT, lift, throwError)
import System.Posix.Types (EpochTime)
import Data.Convertible (convert)

getItems :: (ItemRepo m) => m [Item]
getItems = findItems

getItem :: (ItemRepo m) => Text -> m (Either ItemError Item)
getItem slug = runExceptT $ do
  result <- lift $ findItem slug
  case result of
    [item] -> return item
    _ -> throwError $ ItemErrorNotFound slug
    
getItemsByCategory :: (ItemRepo m) => Text -> m (Either ItemError [Item])
getItemsByCategory category = runExceptT $ do
 result <- lift $ findItemsByCategory category
 case result of
   (_:_) -> return result
   [] -> throwError $ ItemErrorCategoryNotFound category
    
createItem :: (ItemRepo m, TimeRepo m) => ItemIntent -> m (Either ItemError Item)
createItem param = do 
  slug <- genSlug' (itemIntentName param)
  addItem (adaptItem param slug) slug
  getItem slug
  
deleteItem :: (ItemRepo m) => Text -> m (Either ItemError ())
deleteItem slug = runExceptT $ do
  lift $ deleteItemBySlug slug

updateItem :: (ItemRepo m, TimeRepo m) => Text -> ItemIntent -> m (Either ItemError Item)
updateItem slug param = do 
  _ <- getItem slug
  newSlug <- genSlug' (itemIntentName param)
  updateItemBySlug slug param newSlug
  getItem newSlug
  
genSlug' :: (TimeRepo m) => Text -> m Text
genSlug' name = genSlug name ClassyPrelude.. convert <$> currentTime

genSlug :: Text -> EpochTime -> Text
genSlug name unixTs = maybe "invalidSlug" unSlug $ mkSlug $ ClassyPrelude.unwords [tshow unixTs, name]

adaptItem :: ItemIntent -> Text -> Item
adaptItem param slug = Item slug 
                            (itemIntentName param) 
                            (itemIntentDescription param)  
                            (itemIntentCategory param) 
                            (adaptPrice param)
                            (itemIntentImage param)

adaptPrice :: ItemIntent -> Double
adaptPrice param = do fromMaybe 0 (readMaybeDouble (itemIntentPrice param))
    
readMaybeDouble :: String -> Maybe Double
readMaybeDouble = readMay

class (Monad m) => ItemRepo m where
  findItems :: m [Item]
  findItemsByCategory :: Text -> m [Item]
  findItem :: Text -> m [Item]
  deleteItemBySlug :: Text -> m ()
  addItem :: Item -> Text -> m ()
  updateItemBySlug :: Text -> ItemIntent -> Text -> m ()
  
class (Monad m) => TimeRepo m where
  currentTime :: m UTCTime
 