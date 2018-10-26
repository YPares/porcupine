{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Locations.LocVariable where

import           Data.Aeson
import           Data.Hashable       (Hashable)
import           Data.String


-- | Just a variable name
newtype LocVariable = LocVariable String
  deriving (IsString, Show, ToJSON, FromJSON, Eq, Hashable
           ,FromJSONKey, ToJSONKey)
