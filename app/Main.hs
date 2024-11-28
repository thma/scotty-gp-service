{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main (main) where

import           Data.Text                              (pack)
import           Data.Time.Clock
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

validateToken :: ConnectionPool -> TokenValidator
validateToken pool token = do
  let fToken = field "token"
      fExpiry = field "expiry"
  -- Check if the token exists in the BearerToken table and has not expired
  now <- getCurrentTime
  tokens <- withResource pool $ \conn ->
    select @BearerToken conn (fToken =. token &&. fExpiry >. now)
  return $ not (null tokens)

main :: IO ()
main = do
  -- create a connection pool to the SQLite database
  pool <- sqlLitePool "sqlite.db"

  withResource pool $ \conn -> do
    -- initialize Product table
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
    middleware logStdout

    -- Add middleware to authenticate all incoming requests against tokens in the BearerToken table
    middleware $ tokenAuth (validateToken pool)

    get "/all-products" $ do
      products <- withConnFrom pool $ \conn -> select @Product conn  allEntries
      json products

    -- Define a route to list all products with pagination
    get "/products" $ do
      currentPage <- queryParam "page" `catchAny` (\_ -> return 1)
      pageSize <- queryParam "size" `catchAny` (\_ -> return 20)
      let firstPos = (currentPage - 1) * pageSize + 1 :: Int
          lastPos = firstPos + pageSize - 1 :: Int
      page <- withConnFrom pool $ \conn -> select @Product conn (field "id" `between` (firstPos, lastPos))
      totalRecords <- withConnFrom pool $ \conn -> count @Product conn allEntries
      let info = buildPagination totalRecords currentPage pageSize
      json $ ProductList page info

    -- Define a route to get a product by ID
    get "/products/:id" $ do
      productIdParam <- captureParam "id" :: ActionM Int
      prod <- withConnFrom pool $ \conn -> selectById @Product conn productIdParam
      case prod of
        Just p  -> json p
        Nothing -> raiseStatus status404 "not found"

    -- Define a route to create a new product
    post "/products" $ do
      newProduct <- jsonData
      insertedProduct <- withConnFrom pool $ \conn -> insert @Product conn newProduct
      json insertedProduct

    -- Define a route to update a product by ID
    put "/products/:id" $ do
      productIdParam <- captureParam "id"
      updatedProduct <- jsonData
      let updatedProductWithId = updatedProduct {id = productIdParam} :: Product
      updated <- withConnFrom pool $ \conn -> upsert @Product conn updatedProductWithId
      json updated

    -- Define a route to delete a product by ID
    delete "/products/:id" $ do
      productIdParam <- captureParam "id" :: ActionM Int
      deleted <- withConnFrom pool $ \conn -> deleteById @Product conn productIdParam
      json deleted
