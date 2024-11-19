{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models
  ( Product (..),
    Paging (..),
    ProductList (..),
    BearerToken (..),
  )
where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           Database.GP  (Entity (..))
import           GHC.Generics (Generic)

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
data Paging = Paging
  { page       :: Int,
    size       :: Int,
    totalPages :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- Define a combined data type for the product list and paging information
data ProductList = ProductList
  { products :: [Product],
    paging   :: Paging
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data BearerToken = BearerToken
  { id    :: Int,
    token :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

instance Entity BearerToken where
  idField = "id"
