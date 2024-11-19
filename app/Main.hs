{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main (main) where

import           Data.ByteString.Conversion             (toByteString')
import           Data.Text                              hiding (count, elem, map)
import           Database.GP                            hiding (delete)
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
    -- setup table for bearer tokens
    setupTable @BearerToken conn defaultSqliteMapping
    -- insert some valid Tokens
    let initialTokens = [BearerToken 1 "secret", BearerToken 2 "top-secret"]
    insertMany conn initialTokens

  -- Load all valid tokens from the database
  validTokens <- withResource pool $ \conn -> select @BearerToken conn allEntries

  -- Start the web server
  scotty 3000 $ do
    -- Add middleware to log all incoming requests
    middleware logStdout

    -- Add middleware to authenticate all incoming requests against list of valid tokens
    middleware $ tokenListAuth (map (\(BearerToken _ token) -> toByteString' token) validTokens)

    -- Define a route to list all products
    get "/products" $ do
      pageIndex <- queryParam "page" `catchAny` (\_ -> return 1)
      pageSize  <- queryParam "size" `catchAny` (\_ -> return 10)
      let firstPos = (pageIndex - 1) * pageSize + 1 :: Int
          lastPos  = firstPos + pageSize - 1 :: Int
      page     <- withPooledConn $ \conn -> select @Product conn (field "id" `between` (firstPos, lastPos))
      rowCount <- withPooledConn $ \conn -> count @Product conn allEntries
      let pageCount = (rowCount + pageSize - 1) `div` pageSize
          info      = Paging pageIndex pageSize pageCount
      json $ ProductList page info

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
      let updatedProductWithId = updatedProduct {id = productIdParam} :: Product
      updated <- withPooledConn $ \conn -> upsert @Product conn updatedProductWithId
      json updated

    -- Define a route to delete a product by ID
    delete "/products/:id" $ do
      productIdParam <- captureParam "id" :: ActionM Int
      deleted <- withPooledConn $ \conn -> deleteById @Product conn productIdParam
      json deleted
