{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main 
      ( main
      ) where

import Control.Monad (join)
import Control.Applicative ((<$>))

import Core.Item.Controller (routes)
import Core.Item.Controller as ItemController
import Core.Item.Service as ItemService
import Core.Item.DAO as ItemDAO

import Platform.Postgres as Postgres
import qualified Platform.Home as Home

import Data.Maybe (fromMaybe)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Scotty (middleware, scotty)
import Language.Haskell.TH (Type(AppT))
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

instance ItemService.ItemRepo AppT where
  findItems = ItemDAO.findItems
  addItem = ItemDAO.addItem
  findItem = ItemDAO.findItem

instance ItemService.TimeRepo AppT where
  currentTime = liftIO getCurrentTime


