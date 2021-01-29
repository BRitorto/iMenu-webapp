{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main 
      ( main
      ) where
        
import Core.Item.Controller as ItemController
import Core.Item.Service as ItemService
import Core.Item.DAO as ItemDAO

import Core.Category.Controller as CategoryController
import Core.Category.Service as CategoryService
import Core.Category.DAO as CategoryDAO

import Platform.Postgres as Postgres
import qualified Platform.Home as Home
import ClassyPrelude

main :: IO ()
main = do
  pgEnv <- Postgres.init
  let runner app = flip runReaderT pgEnv $ unAppT app
  Home.main runner

type Environment = Postgres.Env

newtype AppT a = AppT
  { unAppT :: ReaderT Environment IO a
  } deriving  (Applicative, Functor, Monad, MonadIO, MonadReader Environment)

instance ItemController.Service AppT where
  getItem = ItemService.getItem
  getItems = ItemService.getItems
  createItem = ItemService.createItem
  getItemsByCategory = ItemService.getItemsByCategory
  deleteItem = ItemService.deleteItem
  updateItem = ItemService.updateItem

instance ItemService.ItemRepo AppT where
  findItems = ItemDAO.findItems
  addItem = ItemDAO.addItem
  findItem = ItemDAO.findItem
  findItemsByCategory = ItemDAO.findItemsByCategory
  deleteItemBySlug = ItemDAO.deleteItemBySlug
  updateItemBySlug = ItemDAO.updateItemBySlug

instance CategoryController.Service AppT where
  createCategory = CategoryService.createCategory

instance CategoryService.CategoryRepo AppT where
  addCategory = CategoryDAO.addCategory
  findCategoryByName = CategoryDAO.findCategoryByName
  categoryExists = CategoryDAO.categoryExists

instance ItemService.TimeRepo AppT where
  currentTime = liftIO getCurrentTime


