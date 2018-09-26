module Data.Locations.VirtualFile
  ( LocationTreePathItem
  , SerializationMethod(..)
  , BidirSerials, PureSerials, PureDeserials
  , JSONSerial(..), PlainTextSerial(..)
  , Profunctor(..)
  , VirtualFile, BidirVirtualFile, DataSource, DataSink
  , vfileUsedByDefault, vfileSerials, vfilePath
  , someBidirSerial, somePureSerial, somePureDeserial
  , customPureSerial, customPureDeserial, makeBidir
  , dataSource, dataSink, virtualFile
  , unusedByDefault
  , vpDeserialToLTPIs, vpSerialToLTPIs
  ) where

import           Data.Locations.LocationTree
import           Data.Locations.SerializationMethod
import           Data.Profunctor                    (Profunctor (..))
import           Data.Void


-- | A virtual path in the location tree to which we can write @a@ and from
-- which we can read @b@.
data VirtualFile a b = VirtualFile
  { vfilePath          :: [LocationTreePathItem]
  , vfileUsedByDefault :: Bool
  , vfileSerials       :: SerialsFor a b }

instance Profunctor VirtualFile where
  dimap f g (VirtualFile l u s) = VirtualFile l u $ dimap f g s

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
virtualFile path sers = VirtualFile path True sers

-- | Indicates that the file should be mapped to 'null' by default
unusedByDefault :: VirtualFile a b -> VirtualFile a b
unusedByDefault vf = vf{vfileUsedByDefault=False}

-- temporary
vpDeserialToLTPIs :: VirtualFile a b -> ([LocationTreePathItem], LTPIAndSubtree SerialMethod)
vpDeserialToLTPIs (VirtualFile [] _ _) = error "vpDeserialToLTPIs: EMPTY PATH"
vpDeserialToLTPIs (VirtualFile p _ s) = (init p, f)
  where
    f = file (last p) (firstPureDeserialFileType s)

-- temporary
vpSerialToLTPIs :: VirtualFile a b -> ([LocationTreePathItem], LTPIAndSubtree SerialMethod)
vpSerialToLTPIs (VirtualFile [] _ _) = error "vpSerialToLTPIs: EMPTY PATH"
vpSerialToLTPIs (VirtualFile p _ s) = (init p, f)
  where
    f = file (last p) (firstPureSerialFileType s)
