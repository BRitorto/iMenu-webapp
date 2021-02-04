{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

module Platform.Home
    ( main
    ) where

import Web.Scotty.Trans
import Network.HTTP.Types.Status
import Network.Wai (Response, Request, pathInfo)
import Network.Wai.Middleware.Cors

import qualified Core.Item.Controller as ItemController
import qualified Core.Category.Controller as CategoryController
import qualified Core.Order.Controller as OrderController

import System.Environment (lookupEnv)
import ClassyPrelude (LText, MonadIO, fromMaybe, readMay)
import Network.Wai.Middleware.HttpAuth (AuthSettings, authIsProtected, basicAuth)

type App r m = (ItemController.Service m, CategoryController.Service m, OrderController.Service m, MonadIO m)

authSettings :: AuthSettings
authSettings = "My Realm" { authIsProtected = needsAuth }

needsAuth :: Request -> IO Bool
needsAuth req = return $ case pathInfo req of
  "admin":_ -> True
  _         -> False

main :: (App r m) => (m Response -> IO Response) -> IO ()
main runner = do
  port <- acquirePort
  scottyT port runner routes
  where
    acquirePort = do
      port <- fromMaybe "" <$> lookupEnv "PORT"
      return . fromMaybe 3000 $ readMay port

routes :: (App r m) => ScottyT LText m ()
routes = do
  -- middlewares
  middleware $ cors $ const $ Just simpleCorsResourcePolicy
    { corsRequestHeaders = ["Authorization", "Content-Type"]
    , corsMethods = "PUT":"DELETE":simpleMethods
    }
    
  -- auth
  middleware $ basicAuth (\u p -> return $ u == "username" && p == "password") authSettings 
  
  -- errors
  defaultHandler $ \str -> do
    status status500
    json str

  -- feature routes
  ItemController.routes
  ItemController.adminRoutes
  CategoryController.routes
  CategoryController.adminRoutes
  OrderController.adminRoutes
  OrderController.routes
  
  -- health
  get "/api/health" $
    json True
    