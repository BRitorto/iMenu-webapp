{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Platform.Postgres where

import ClassyPrelude
import Data.Has
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration
import System.Environment
import Data.Pool

type Env = Pool Connection

type Postgres r m = (MonadReader r m, Has Env r, MonadIO m)
    
init :: IO Env
init = do
  pool <- acquirePool
  migrateDb pool
  return pool

acquirePool :: IO (Pool Connection)
acquirePool = do
  envUrl <- lookupEnv "DATABASE_URL"
  let pgUrl = fromString $ fromMaybe "postgresql://localhost/postgres" envUrl
  createPool (connectPostgreSQL pgUrl) close 1 10 10

migrateDb :: Pool Connection -> IO ()
migrateDb pool = withResource pool $ \conn ->
  void $ withTransaction conn (runMigration (ctx conn))
  where
    ctx = MigrationContext cmd False
    cmd = MigrationCommands [ MigrationInitialization, MigrationDirectory "postgresql" ]

withConn :: Postgres r m => (Connection -> IO a) -> m a
withConn action = do
  pool <- asks getter
  liftIO $ withResource pool action


