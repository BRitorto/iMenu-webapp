{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Item.DAO where

import Database.PostgreSQL.Simple

import Core.Item.Types
import Platform.Postgres
import ClassyPrelude (Text, void)

foo :: (a -> Either [Char] (c, b)) -> a -> c
foo f = fst . either error id . f

addItem :: Postgres r m => Item -> Text -> m ()
addItem param slug =
  void . withConn $ \conn -> execute conn qry 
    (slug, itemName param, itemDescription param, itemCategory param, itemPrice param,  itemImage param)
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