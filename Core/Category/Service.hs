{-# LANGUAGE AllowAmbiguousTypes #-}

module Core.Category.Service where

import ClassyPrelude (Text, lift)
import Core.Category.Types
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)

createCategory :: (CategoryRepo m) => Text -> m (Either CategoryError Category)
createCategory name = do
  addCategory name
  getCategory name
  
getCategory :: (CategoryRepo m) => Text -> m (Either CategoryError Category)
getCategory name = runExceptT $ do 
  result <- lift $ findCategoryByName name
  case result of
    [category] -> return category
    _ -> throwError $ CategoryNotFound name
  
class (Monad m) => CategoryRepo m where
    addCategory :: Text -> m()
    findCategoryByName :: Text -> m [Category]
    categoryExists :: Text -> m (Maybe Bool)