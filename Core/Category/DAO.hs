{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Category.DAO where

import ClassyPrelude (Text, void)
import Platform.Postgres
import Database.PostgreSQL.Simple
import Core.Category.Types

addCategory :: Postgres r m => Text -> m ()
addCategory param =
  void . withConn $ \conn -> execute conn qry (Only param)
  where
    qry = "insert into categories (name, created_at) values (?, now())"

findCategoryByName :: Postgres r m => Text -> m [Category]
findCategoryByName name = 
  withConn $ \conn -> query conn qry (Only name)
  where qry = "select \
                   \ name \
               \ from \
                   \ categories \
               \ where \
                   \ name = ?"

categoryExists :: Postgres r m => Text -> m (Maybe Bool)
categoryExists name = do
  results <- withConn $ \conn -> query conn qry (Only name)
  case results of
      [Category _] -> return $ Just True
      _ -> return $ Just False
  where
    qry = "select name from categories where name = ? limit 1"