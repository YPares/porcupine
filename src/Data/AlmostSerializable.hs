{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

-- |
-- Classes for datatypes which aren't serializable, but can be (possibly lossy)
-- converted from or to a serializable type
module Data.AlmostSerializable
  ( Writable(..)
  , Readable(..)
  , Serializable
  ) where

import qualified Data.HumanSerializable as HS

-- |
-- Types which can be converted to a writeable artifact
class HS.ToHuman (WrtArtifact a) => Writable a where
  type WrtArtifact a :: *
  toWritable :: a -> WrtArtifact a

-- |
-- Types which can be converted from a readable artifact
class HS.OfHuman (ReadArtifact a) => Readable a where
  type ReadArtifact a :: *
  fromReadable :: ReadArtifact a -> a

-- |
-- Types which can be converted to/from a readwrite artifact
type Serializable a = (Writable a, Readable a, WrtArtifact a ~ ReadArtifact a)
