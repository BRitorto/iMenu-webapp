{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Order.DAO where

import Platform.Postgres
import Core.Order.Types
import ClassyPrelude (void)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types

addOrder :: Postgres r m => OrderIntent -> m ()
addOrder param =
  void . withConn $ \conn -> execute conn qry (orderIntentTableNumber param, PGArray (orderIntentItems param))
  where
    qry = "insert into orders (table_number, items, created_at, updated_at) values (?, ?, now(), now())"

findOrderByTable :: Postgres r m => Integer -> m [Order]
findOrderByTable table =
  withConn $ \conn -> query conn qry (Only table)
  where qry = "select \
                   \ orders.id, table_number, items \
               \ from \
                   \ orders \
               \ where \
                   \ table_number = ?"

deleteOrderByTable :: Postgres r m => Integer -> m ()
deleteOrderByTable table =
  void . withConn $ \conn -> execute conn qry (Only table)
    where
      qry = "delete from orders where table_number = ?"
      
findOrders :: Postgres r m => m [Order]
findOrders = do 
  withConn $ \conn -> query_ conn qry
  where qry = "select \
                 \ orders.id, table_number, items\
             \ from \
                 \ orders"