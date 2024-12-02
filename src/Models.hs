{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models
  ( Product (..),
    Pagination (..),
    buildPagination,
    ProductList (..),
    BearerToken (..),
  )
where

import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Text       (Text)
import           Data.Time.Clock (UTCTime)
import           Database.GP     (Entity (..))
import           GHC.Generics    (Generic)

-- Define a Product data type
data Product = Product
  { id          :: Int,
    name        :: Text,
    description :: Text,
    price       :: Double
  }
  deriving (Show, Generic, ToJSON, FromJSON)

instance Entity Product where
  idField = "id"

-- Define a Paging information data type
data Pagination = Pagination
  { totalRecords :: Int,
    currentPage  :: Int,
    totalPages   :: Int,
    nextPage     :: Maybe Int,
    prevPage     :: Maybe Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Helper function to build pagination information
--   totalRecords: total number of records
--   currentPage: current page number
--   pageSize: number of records per page
--   returns: Pagination information
buildPagination :: Int -> Int -> Int -> Pagination
buildPagination totalRecords currentPage pageSize =
  let totalPages = (totalRecords + pageSize - 1) `div` pageSize
      nextPage
        | currentPage < 1 = Just 1
        | currentPage < totalPages = Just (currentPage + 1)
        | otherwise = Nothing
      prevPage
        | currentPage > totalPages = Just totalPages
        | currentPage > 1 = Just (currentPage - 1)
        | otherwise = Nothing
   in Pagination totalRecords currentPage totalPages nextPage prevPage

-- Define a combined data type for the product list and paging information
data ProductList = ProductList
  { products   :: [Product],
    pagination :: Pagination
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data BearerToken = BearerToken
  { token  :: Text,
    expiry :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON)

instance Entity BearerToken where
  idField = "token"
  autoIncrement = False
