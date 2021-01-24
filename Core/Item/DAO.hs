{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Item.DAO where

import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types

import Core.Item.Types
import Platform.Postgres
import Web.Slug (Slug)
import ClassyPrelude (void, maybeToList, Text, setFromList)

addItem :: Postgres r m => Item -> Text -> m ()
addItem param slug =
  void . withConn $ \conn -> execute conn qry 
    ( slug, itemName param, itemDescription param, itemCategory param
    , itemPrice param, itemImage param
    )
  where
    qry = "insert into items (slug, name, description, category, price, image, created_at, updated_at) \
          \values (?, ?, ?, ?, ?, ?, now(), now())"

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
    --arg = ( In $ maybeToList maySlug
         -- , In $ maybeToList $ itemCategoryFilter itemFilter)
          