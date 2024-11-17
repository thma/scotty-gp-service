{-# LANGUAGE OverloadedStrings   #-}

module Main (main) where

import           Control.Monad.IO.Class (liftIO)
import           Models
import           Network.HTTP.Types     (status404)
import           Web.Scotty             (scotty, get, captureParam, post, put, jsonData, json, raiseStatus, ActionM)
import           Database.GP
import           Database.HDBC.Sqlite3 (connectSqlite3)

-- Sample product list
initialProducts :: [Product]
initialProducts =
  [ Product 1 "Product 1" "Description 1" 19.99
  , Product 2 "Product 2" "Description 2" 29.99
  ]

main :: IO ()
main = do
  -- connect to a database in auto commit mode
  conn <- connect AutoCommit <$> connectSqlite3 "sqlite.db"

  -- initialize Product table
  _ <- setupTableFor @Product SQLite conn

  -- insert initial products
  _ <- insertMany conn initialProducts
  
  -- Start the web server
  scotty 3000 $ do
    -- Define a route to list all products
    get "/products" $ do
      products <- liftIO $ select @Product conn allEntries
      json products

    -- Define a route to get a product by ID
    get "/products/:id" $ do
      productIdParam <- captureParam "id"
      prod <- liftIO $ selectById @Product conn (productIdParam :: Int)
      case prod of
       Just p  -> json p
       Nothing -> raiseStatus status404 "not found"

    -- Define a route to create a new product
    post "/products" $ do
      newProduct <- jsonData
      insertedProduct <- liftIO $ insert @Product conn newProduct
      json insertedProduct

    -- Define a route to update a product by ID
    put "/products/:id" $ do
      --productIdParam <- captureParam "id"
      updatedProduct <- jsonData
      --let updatedProductWithId = updatedProduct { Models.id = read productIdParam }
      updated <- liftIO $ update @Product conn updatedProduct
      json updated


