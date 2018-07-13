{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -Wall #-}

module Data.HumanSerializable
  ( ToHuman(..), OfHuman(..), Serializable
  ) where

import qualified Data.Aeson           as JSON
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T

class ToHuman a where
  encode :: a -> LBS.ByteString
  default encode :: JSON.ToJSON a => a -> LBS.ByteString
  encode = JSON.encode

class OfHuman a where
  decode :: LBS.ByteString -> Either T.Text a
  default decode :: JSON.FromJSON a => LBS.ByteString -> Either T.Text a
  decode x =
    case JSON.eitherDecode x of
      Left msg  -> Left $ T.pack msg
      Right elt -> Right elt

-- | Class of values which can be serialized in a human-readable form
-- (json, csv, whatever…)
type Serializable a = (ToHuman a, OfHuman a)
