{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main (main) where

import           Data.Text                              (pack)
import           Data.Time.Clock
import           Database.GP                            hiding (delete)
import           Database.GP.SqlGenerator
import           Database.HDBC.Sqlite3                  (connectSqlite3)
import           Models
import           Network.HTTP.Types                     (status404)
import           Network.Wai.Middleware.BearerTokenAuth
import           Network.Wai.Middleware.RequestLogger
import           UnliftIO.Exception
import           Web.Scotty

-- Sample product list
initialProducts :: [Product]
initialProducts = map toProduct [1 .. 100]
  where
    toProduct i = Product i ("Product " <> pack (show i)) ("Description " <> pack (show i)) (19.99 + fromIntegral i * 10)

-- Create a connection pool to a SQLite database specified by its file path
sqlLitePool :: FilePath -> IO ConnectionPool
sqlLitePool sqlLiteFile = createConnPool AutoCommit sqlLiteFile connectSqlite3 10 100

-- validate a token against the BearerToken table.
-- It takes a ConnectionPool (used for db access) as an argument
-- and returns a TokenValidator function.
-- The TokenValidator function takes a token string as an argument
-- and returns a IO Bool indicating whether the token is valid.
-- The token is valid if it exists in the BearerToken table and its expiry date is in the future.
validateToken :: ConnectionPool -> TokenValidator
validateToken pool token = do
  now <- getCurrentTime                                   -- get the current time
  tokens <- withResource pool $ \conn ->                  -- use a connection from the pool
    select @BearerToken conn                              -- to select tokens from the BearerToken table    
      (field "token" =. token &&. field "expiry" >. now)  -- where the token matches and expiry is in the future
  return $ not (null tokens)                              -- return True if a valid token exists

main :: IO ()
main = do
  -- create a connection pool to the SQLite database
  pool <- sqlLitePool "sqlite.db"
  -- define Helper function to run a database action with a connection from the pool
  let withPooledConn = liftIO . withResource pool

  withResource pool $ \conn -> do
    -- initialize Product table
    print $ "DDL statement for Product table: " ++ createTableStmtFor @Product defaultSqliteMapping
    setupTable @Product conn defaultSqliteMapping
    -- insert initial products
    insertMany conn initialProducts
    -- setup table for bearer tokens
    setupTable @BearerToken conn defaultSqliteMapping
    -- insert some tokens with different expiry dates
    now <- getCurrentTime
    let oneDay = addUTCTime nominalDay now
    let initialTokens =
          [ BearerToken "top-secret" oneDay, -- expires in 1 day
            BearerToken "secret" now -- expires immediately
          ]
    insertMany conn initialTokens

  -- Start the web server
  scotty 3000 $ do
    -- Add middleware to log all incoming requests
    middleware logStdoutDev

    -- Add middleware to authenticate all incoming requests against tokens in the BearerToken table
    middleware $ tokenAuth (validateToken pool)

    -- Define a route to list all products by performing a select query on the Product table.
    get "/all-products" $ do
      products <- withPooledConn $ \conn ->
        select @Product conn allEntries
      json products

    -- Define a route to list all products with pagination
    get "/products" $ do
      currentPage <- queryParam "page" `catchAny` (\_ -> return 1)
      pageSize <- queryParam "size" `catchAny` (\_ -> return 20)
      let offset = (currentPage - 1) * pageSize :: Int
      page <- withPooledConn $ \conn ->
        select @Product conn (allEntries `limitOffset` (offset, pageSize))
      totalRecords <- withPooledConn $ \conn ->
        count @Product conn allEntries
      let info = buildPagination totalRecords currentPage pageSize
      json $ ProductList page info

    -- Define a route to get a product by ID
    get "/products/:id" $ do
      productIdParam <- captureParam "id" :: ActionM Int
      prod <- withPooledConn $ \conn ->
        selectById @Product conn productIdParam
      case prod of
        Just p  -> json p
        Nothing -> raiseStatus status404 "not found"

    -- Define a route to create a new product
    post "/products" $ do
      newProduct <- jsonData
      insertedProduct <- withPooledConn $ \conn ->
        insert @Product conn newProduct
      json insertedProduct

    -- Define a route to update a product by ID
    put "/products/:id" $ do
      productIdParam <- captureParam "id"
      updatedProduct <- jsonData
      let updatedProductWithId = updatedProduct {id = productIdParam} :: Product
      updated <- withPooledConn $ \conn ->
        upsert @Product conn updatedProductWithId
      json updated

    -- Define a route to delete a product by ID
    delete "/products/:id" $ do
      productIdParam <- captureParam "id" :: ActionM Int
      deleted <- withPooledConn $ \conn ->
        deleteById @Product conn productIdParam
      json deleted
