{-# LANGUAGE TypeOperators #-}

module Data.Locations.VirtualFile
  ( LocationTreePathItem
  , SerializationMethod(..)
  , BidirSerials, PureSerials, PureDeserials
  , JSONSerial(..), PlainTextSerial(..)
  , Profunctor(..)
  , VirtualFile, BidirVirtualFile, DataSource, DataSink
  , vfileUsedByDefault, vfileSerials, vfilePath, vfileBidirProof
  , someBidirSerial, somePureSerial, somePureDeserial
  , customPureSerial, customPureDeserial, makeBidir
  , dataSource, dataSink, bidirVirtualFile
  , documentedFile, unusedByDefault
  , makeSink, makeSource
  ) where

import           Data.Locations.LocationTree
import           Data.Locations.SerializationMethod
import           Data.Monoid                        (First (..))
import           Data.Profunctor                    (Profunctor (..))
import qualified Data.Text                          as T
import           Data.Type.Equality
import           Data.Void


-- | A virtual path in the location tree to which we can write @a@ and from
-- which we can read @b@.
data VirtualFile a b = VirtualFile
  { vfilePath          :: [LocationTreePathItem]
  , vfileUsedByDefault :: Bool
  , vfileDocumentation :: First T.Text
  , vfileBidirProof    :: First (a :~: b)
                       -- Temporary, necessary until we can do away with docrec
                       -- conversion in the writer part of SerialsFor
  , vfileSerials       :: SerialsFor a b }

instance Semigroup (VirtualFile a b) where
  VirtualFile p u d b s <> VirtualFile _ u' d' b' s' =
    VirtualFile p (u && u') (d<>d') (b<>b') (s<>s')

fn :: First a
fn = First Nothing

instance Profunctor VirtualFile where
  dimap f g (VirtualFile p u d _ s) = VirtualFile p u d fn $ dimap f g s

-- | A virtual file which depending on the situation can be written or read
type BidirVirtualFile a = VirtualFile a a

-- | A virtual file that's only readable
type DataSource a = VirtualFile Void a

-- | A virtual file that's only writable
type DataSink a = VirtualFile a ()

-- | Creates a virtual file from its virtual path and ways to deserialize the
-- data.
dataSource :: [LocationTreePathItem] -> SerialsFor a b -> DataSource b
dataSource path = virtualFile path . eraseSerials

-- | Creates a virtual file from its virtual path and ways to serialize the
-- data.
dataSink :: [LocationTreePathItem] -> SerialsFor a b -> DataSink a
dataSink path = virtualFile path . eraseDeserials

-- | Creates a virtuel file from its virtual path and ways serialize/deserialize
-- the data. You should prefer 'dataSink' and 'dataSource' for clarity when the
-- file is meant to be readonly or writeonly.
virtualFile :: [LocationTreePathItem] -> SerialsFor a b -> VirtualFile a b
virtualFile path sers = VirtualFile path True fn fn sers

-- | Like VirtualFile, except we will embed the proof that @a@ and @b@ are the same
bidirVirtualFile :: [LocationTreePathItem] -> BidirSerials a -> BidirVirtualFile a
bidirVirtualFile path sers = VirtualFile path True fn (First $ Just Refl) sers

-- | Indicates that the file should be mapped to 'null' by default
unusedByDefault :: VirtualFile a b -> VirtualFile a b
unusedByDefault vf = vf{vfileUsedByDefault=False}

makeSink :: VirtualFile a b -> DataSink a
makeSink vf = vf{vfileSerials=eraseDeserials $ vfileSerials vf
                ,vfileBidirProof=fn}

makeSource :: VirtualFile a b -> DataSource b
makeSource vf = vf{vfileSerials=eraseSerials $ vfileSerials vf
                  ,vfileBidirProof=fn}

documentedFile :: VirtualFile a b -> T.Text -> VirtualFile a b
documentedFile vf doc = vf{vfileDocumentation = First $ Just doc}
