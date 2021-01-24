{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

module Platform.Home
    ( main
    ) where

import ClassyPrelude (MonadIO, LText, fromMaybe, readMay)
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai (Response)
import Network.Wai.Middleware.Cors

import qualified Core.Item.Controller as ItemController
import System.Environment (lookupEnv)

type App r m = (ItemController.Service m, MonadIO m)

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
    { corsRequestHeaders = "Authorization":simpleHeaders
    , corsMethods = "PUT":"DELETE":simpleMethods
    }
  options (regex ".*") $ return ()

  -- errors
  defaultHandler $ \str -> do
    status status500
    json str

  -- feature routes
  ItemController.routes
  
  -- health
  get "/api/health" $
    json True