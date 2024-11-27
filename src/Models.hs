{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models
  ( Product (..),
    Pagination (..),
    ProductList (..),
    BearerToken (..),
  )
where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           Database.GP  (Entity (..))
import           GHC.Generics (Generic)
import           Data.Time.Clock ( UTCTime )

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

-- Define a combined data type for the product list and paging information
data ProductList = ProductList
  { products :: [Product],
    paging   :: Pagination
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data BearerToken = BearerToken
  { id :: Int,
    token  :: Text,
    expiry :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON)

instance Entity BearerToken where
  idField = "id"
