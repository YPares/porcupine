{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}

module Data.Locations.VirtualFile
  ( LocationTreePathItem
  , SerializationMethod(..)
  , BidirSerials, PureSerials, PureDeserials
  , JSONSerial(..), PlainTextSerial(..)
  , Profunctor(..)
  , VirtualFile, BidirVirtualFile, DataSource, DataSink
  , VirtualFileInTree
  , VirtualFileOrData  -- temporary
  , VirtualFileNode, pattern VirtualFileNode
  , DataAccessNode, pattern DataAccessNode
  , vfileUsedByDefault, vfileSerials, vfilePath, vfileBidirProof
  , someBidirSerial, somePureSerial, somePureDeserial
  , customPureSerial, customPureDeserial, makeBidir
  , dataSource, dataSink, bidirVirtualFile
  , makeSink, makeSource
  , documentedFile, unusedByDefault
  , removeVFilePath
  ) where

import           Control.Lens
import           Data.Locations.LocationTree
import           Data.Locations.Mappings
import           Data.Locations.SerializationMethod
import           Data.Monoid                        (First (..))
import           Data.Profunctor                    (Profunctor (..))
import qualified Data.Text                          as T
import           Data.Typeable
import           Data.Type.Equality
import           Data.Void


-- | A virtual file in the location tree to which we can write @a@ and from
-- which we can read @b@.
data VirtualFile_ d a b = VirtualFile
  { _vfileStateData :: d
  , vfileBidirProof :: First (a :~: b)
                    -- Temporary, necessary until we can do away with docrec
                    -- conversion in the writer part of SerialsFor
  , vfileSerials    :: SerialsFor a b }

vfileStateData :: Lens (VirtualFile_ d a b) (VirtualFile_ d' a b) d d'
vfileStateData f vf = (\d' -> vf{_vfileStateData=d'}) <$> f (_vfileStateData vf)

instance Semigroup d => Semigroup (VirtualFile_ d a b) where
  VirtualFile d b s <> VirtualFile d' b' s' =
    VirtualFile (d<>d') (b<>b') (s<>s')
instance Monoid d => Monoid (VirtualFile_ d a b) where
  mempty = VirtualFile mempty mempty mempty

fn :: First a
fn = First Nothing

instance Profunctor (VirtualFile_ d) where
  dimap f g (VirtualFile d _ s) = VirtualFile d fn $ dimap f g s

data VFMetadata = VFMetadata
  { _vfileMD_UsedByDefault :: Bool
  , _vfileMD_Documentation :: First T.Text }

makeLenses ''VFMetadata

instance Semigroup VFMetadata where
  VFMetadata u d <> VFMetadata u' d' = VFMetadata (u && u') (d<>d')

-- | Used before the VirtualFile is placed in the LocationTree, to know where it
-- should go.
data VFMetadataWithPath = VFMetadataWithPath
  { _vfileState_Path :: [LocationTreePathItem]
  , _vfileState_MD   :: VFMetadata }

makeLenses ''VFMetadataWithPath

-- | A VirtualFile, as declared by an application.
type VirtualFile = VirtualFile_ VFMetadataWithPath

-- | A VirtualFile without its path. Becomes a Semigroup.
type VirtualFileInTree = VirtualFile_ VFMetadata

removeVFilePath :: VirtualFile a b -> VirtualFileInTree a b
removeVFilePath vf = vf & vfileStateData %~ view vfileState_MD

vfilePath :: VirtualFile a b -> [LocationTreePathItem]
vfilePath = view (vfileStateData . vfileState_Path)

vfileUsedByDefault :: VirtualFile a b -> Bool
vfileUsedByDefault = view (vfileStateData . vfileState_MD . vfileMD_UsedByDefault)

-- | A virtual file which depending on the situation can be written or read
type BidirVirtualFile a = VirtualFile a a

-- | A virtual file that's only readable
type DataSource a = VirtualFile Void a

-- | A virtual file that's only writable
type DataSink a = VirtualFile a ()


-- | Creates a virtuel file from its virtual path and ways serialize/deserialize
-- the data. You should prefer 'dataSink' and 'dataSource' for clarity when the
-- file is meant to be readonly or writeonly.
virtualFile :: [LocationTreePathItem] -> Maybe (a :~: b) -> SerialsFor a b -> VirtualFile a b
virtualFile path refl sers =
  VirtualFile (VFMetadataWithPath path $ VFMetadata True fn) (First refl) sers

-- | Creates a virtual file from its virtual path and ways to deserialize the
-- data.
dataSource :: [LocationTreePathItem] -> SerialsFor a b -> DataSource b
dataSource path = virtualFile path Nothing . eraseSerials

-- | Creates a virtual file from its virtual path and ways to serialize the
-- data.
dataSink :: [LocationTreePathItem] -> SerialsFor a b -> DataSink a
dataSink path = virtualFile path Nothing . eraseDeserials

-- | Like VirtualFile, except we will embed the proof that @a@ and @b@ are the same
bidirVirtualFile :: [LocationTreePathItem] -> BidirSerials a -> BidirVirtualFile a
bidirVirtualFile path sers = virtualFile path (Just Refl) sers

makeSink :: VirtualFile a b -> DataSink a
makeSink vf = vf{vfileSerials=eraseDeserials $ vfileSerials vf
                ,vfileBidirProof=fn}

makeSource :: VirtualFile a b -> DataSource b
makeSource vf = vf{vfileSerials=eraseSerials $ vfileSerials vf
                  ,vfileBidirProof=fn}


-- | Indicates that the file should be mapped to 'null' by default
unusedByDefault :: VirtualFile a b -> VirtualFile a b
unusedByDefault = vfileStateData . vfileState_MD . vfileMD_UsedByDefault .~ False

-- | Gives a documentation to the 'VirtualFile'
documentedFile :: T.Text -> VirtualFile a b -> VirtualFile a b
documentedFile doc = vfileStateData . vfileState_MD . vfileMD_Documentation .~ First (Just doc)


-- | The internal part of a 'VirtualFileNode', closing over the type params of
-- the 'VirtualFile'
data VirtualFileNodeI where
  VirtualFileNodeI :: (Typeable a, Typeable b, Monoid b) => VirtualFileInTree a b -> VirtualFileNodeI

instance Semigroup VirtualFileNodeI where
  VirtualFileNodeI vf <> VirtualFileNodeI vf' = case cast vf' of
    Just vf'' -> VirtualFileNodeI $ vf <> vf''
    Nothing -> error "Two differently typed VirtualFiles are at the same location"

-- | The internal part of a 'DataAccessNode, closing over the type params of the
-- access function
data DataAccessNodeI m where
  DataAccessNodeI :: (Typeable a, Typeable b) => (a -> m b) -> DataAccessNodeI m

-- The end type param is only compatibility with ATask. Will be removed in the
-- future when ATask is modified, and these aliases modified.

-- | The nodes of the LocationTree when using VirtualFiles
type VirtualFileNode m = VirtualFileOrData m WithDefaultUsage

-- | The nodes of the LocationTree after the VirtualFiles have been resolved to
-- physical paths, and data possibly extracted from these paths
type DataAccessNode m = VirtualFileOrData m LocLayers

data VirtualFileOrData m f where
  VirtualFileNodeE :: Maybe VirtualFileNodeI -> VirtualFileNode m
  DataAccessNodeE  :: First (DataAccessNodeI m) -> DataAccessNode m

-- pattern VirtualFileNode
--   :: (Typeable a, Typeable b, Monoid b) => VirtualFileInTree a b -> VirtualFileNode m
pattern VirtualFileNode x = VirtualFileNodeE (Just (VirtualFileNodeI x))

-- pattern DataAccessNode
--   :: (Typeable a, Typeable b) => (a -> m b) -> DataAccessNode m
pattern DataAccessNode x = DataAccessNodeE (First (Just (DataAccessNodeI x)))

-- TODO: It is dubious that composing DataAccessNodes is really needed in the
-- end. Find a way to remove that.

instance Semigroup (VirtualFileOrData m f) where
  VirtualFileNodeE vf <> VirtualFileNodeE vf' = VirtualFileNodeE $ vf <> vf'
  DataAccessNodeE f <> DataAccessNodeE f' = DataAccessNodeE $ f <> f'
instance Monoid (VirtualFileOrData m WithDefaultUsage) where
  mempty = VirtualFileNodeE mempty
instance Monoid (VirtualFileOrData m LocLayers) where
  mempty = DataAccessNodeE mempty
