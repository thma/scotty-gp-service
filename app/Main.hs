{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main (main) where

import           Control.Monad.IO.Class (liftIO)
import           Database.GP            hiding (delete)
import           Database.HDBC.Sqlite3  (connectSqlite3)
import           Models
import           Network.HTTP.Types     (status404)
import           Web.Scotty             (ActionM, captureParam, delete, get,
                                         json, jsonData, post, put, raiseStatus,
                                         scotty)
                                                  
-- Sample product list
initialProducts :: [Product]
initialProducts =
  [ Product 1 "Product 1" "Description 1" 19.99,
    Product 2 "Product 2" "Description 2" 29.99
  ]

-- Create a connection pool to the SQLite database
sqlLitePool :: FilePath -> IO ConnectionPool
sqlLitePool sqlLiteFile = createConnPool AutoCommit sqlLiteFile connectSqlite3 10 100  

-- Helper function to run a database action with a connection from the pool
withConnFrom :: ConnectionPool -> (Conn -> IO a) -> ActionM a
withConnFrom pool gpAction = liftIO $ withResource pool gpAction

main :: IO ()
main = do
  -- create a connection pool to the SQLite database
  pool <- sqlLitePool "sqlite.db"
  let withPooledConn = withConnFrom pool

  withResource pool $ \conn -> do
    -- initialize Product table
    setupTable @Product conn defaultSqliteMapping
    -- insert initial products
    insertMany conn initialProducts

  -- Start the web server
  scotty 3000 $ do
    -- Define a route to list all products
    get "/products" $ do
      products <- withPooledConn $ \conn -> select @Product conn allEntries
      json products

    -- Define a route to get a product by ID
    get "/products/:id" $ do
      productIdParam <- captureParam "id" :: ActionM Int
      prod <- withPooledConn $ \conn -> selectById @Product conn productIdParam
      case prod of
        Just p  -> json p
        Nothing -> raiseStatus status404 "not found"

    -- Define a route to create a new product
    post "/products" $ do
      newProduct <- jsonData
      insertedProduct <- withPooledConn $ \conn -> insert @Product conn newProduct
      json insertedProduct

    -- Define a route to update a product by ID
    put "/products/:id" $ do
      productIdParam <- captureParam "id"
      updatedProduct <- jsonData
      let updatedProductWithId = updatedProduct {id = productIdParam}
      updated <- withPooledConn $ \conn -> upsert @Product conn updatedProductWithId
      json updated

    -- Define a route to delete a product by ID
    delete "/products/:id" $ do
      productIdParam <- captureParam "id" :: ActionM Int
      deleted <- withPooledConn $ \conn -> deleteById @Product conn productIdParam
      json deleted

