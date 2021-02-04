{-# LANGUAGE AllowAmbiguousTypes #-}

module Core.Category.Service where

import ClassyPrelude (Text, lift)
import Core.Category.Types
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except

createCategory :: (CategoryRepo m) => Text -> m (Either CategoryError Category)
createCategory name = runExceptT $ do
  ExceptT $ validateCategoryNotExists name
  lift $ addCategory name
  ExceptT $ getCategory name
  
validateCategoryNotExists :: (CategoryRepo m ) => Text -> m (Either CategoryError ())
validateCategoryNotExists param = runExceptT $ do
  result <- lift $ categoryExists param
  case result of
   Just True -> throwError $ CategoryDuplicated param
   _ -> return ()
  
getCategory :: (CategoryRepo m) => Text -> m (Either CategoryError Category)
getCategory name = runExceptT $ do 
  result <- lift $ findCategoryByName name
  case result of
    [category] -> return category
    _ -> throwError $ CategoryNotFound name
  
getCategories :: (CategoryRepo m) => m [Category]
getCategories = findCategories

class (Monad m) => CategoryRepo m where
    addCategory :: Text -> m ()
    findCategoryByName :: Text -> m [Category]
    categoryExists :: Text -> m (Maybe Bool)
    findCategories :: m [Category]