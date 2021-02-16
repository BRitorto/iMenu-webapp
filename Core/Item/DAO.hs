{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Core.Item.DAO where

import Database.PostgreSQL.Simple

import Core.Item.Types
import Platform.Postgres
import ClassyPrelude (Text, void)

addItem :: Postgres r m => ItemIntent -> Text -> m ()
addItem param slug =
  void . withConn $ \conn -> execute conn qry 
    (slug, itemIntentName param, itemIntentDescription param, itemIntentCategory param, itemIntentPrice param,  itemIntentImage param)
  where
    qry = "insert into items (slug, name, description, category, price, image, created_at, updated_at) \
          \values (?, ?, ?, ?, ?, ?, now(), now())"

findItem :: Postgres r m => Text -> m [Item]
findItem slug =
  withConn $ \conn -> query conn qry (Only slug)
  where qry = "select \
                   \ slug, name, description, category, price, image \
               \ from \
                   \ items \
               \ where \
                   \ slug = ?"

findItemsByCategory :: Postgres r m => Text -> m [Item]
findItemsByCategory category =
  withConn $ \conn -> query conn qry (Only category)
  where qry = "select \
                   \ slug, name, description, category, price, image \
               \ from \
                   \ items \
               \ where \
                   \ category = ? \
               \ order by name desc \
               \ limit greatest(0, 20)"

findItems :: Postgres r m => m [Item]
findItems = do 
  withConn $ \conn -> query_ conn qry
  where qry = "with formatted_items as ( \
              \ select \
                  \ items.id, slug, name, description, category, price, image, created_at, updated_at \
              \ from \
                \ items ) \
              \ select \
                   \ slug, name, description, category, price, image \
              \ from \
                   \ formatted_items \
              \ order by id desc \
              \ limit greatest(0, 20)"

deleteItemBySlug :: Postgres r m => Text -> m ()
deleteItemBySlug slug =
  void . withConn $ \conn -> execute conn qry (Only slug)
    where
      qry = "delete from items where slug = ?"

updateItemBySlug :: Postgres r m => Text -> ItemIntent -> Text -> m ()
updateItemBySlug slug param newSlug =
  void . withConn $ \conn -> execute conn qry
      (newSlug, itemIntentName param, itemIntentDescription param, itemIntentCategory param, itemIntentPrice param,  itemIntentImage param, slug)
  where
    qry = "update items \
          \set slug = ?, name = coalesce(?, name), description = coalesce(?, description), \
          \    category = coalesce(?, category), price = coalesce(?, price), image = coalesce(?, image), updated_at = now() \
          \where slug = ?"

itemsExist :: Postgres r m => [Text] -> m (Maybe Bool)
itemsExist slugs = do
  results <- withConn $ \conn -> execute conn qry slugs
  if fromIntegral results == length slugs then return $ Just True else return $ Just False
  where
    qry = "select count(distinct slug) from items where slug in ?"