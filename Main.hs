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

import Core.Order.Controller as OrderController
import Core.Order.Service as OrderService
import Core.Order.DAO as OrderDAO

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
  itemsExist = ItemDAO.itemsExist

instance CategoryController.Service AppT where
  createCategory = CategoryService.createCategory
  getCategories = CategoryService.getCategories

instance CategoryService.CategoryRepo AppT where
  addCategory = CategoryDAO.addCategory
  findCategoryByName = CategoryDAO.findCategoryByName
  categoryExists = CategoryDAO.categoryExists
  findCategories = CategoryDAO.findCategories

instance OrderController.Service AppT where
  getOrder = OrderService.getOrder
  getOrders = OrderService.getOrders
  createOrder = OrderService.createOrder
  finishOrder = OrderService.finishOrder
  
instance OrderService.OrderRepo AppT where
  addOrder = OrderDAO.addOrder
  findOrderByTable = OrderDAO.findOrderByTable
  deleteOrderByTable = OrderDAO.deleteOrderByTable
  findOrders = OrderDAO.findOrders

instance ItemService.TimeRepo AppT where
  currentTime = liftIO getCurrentTime


