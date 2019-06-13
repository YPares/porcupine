{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Locations.LocVariable where

import           Data.Aeson
import           Data.Hashable (Hashable)
import           Data.Store    (Store)
import           Data.String


-- | Just a variable name
newtype LocVariable = LocVariable { unLocVariable :: String }
  deriving (IsString, Show, ToJSON, FromJSON, Eq, Hashable
           ,FromJSONKey, ToJSONKey, Store)
