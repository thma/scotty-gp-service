{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models
  ( Product (..),
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
