{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module System.TaskPipeline.ResourceTree where

import Data.Typeable
import           Data.Locations.Mappings
import Data.Locations.VirtualFile
import           Data.Monoid                        (First (..))


-- | The internal part of a 'VirtualFileNode', closing over the type params of
-- the 'VirtualFile'
data VirtualFileNodeI where
  VirtualFileNodeI :: (Typeable a, Typeable b, Monoid b) => PathlessVirtualFile a b -> VirtualFileNodeI

instance Semigroup VirtualFileNodeI where
  VirtualFileNodeI vf <> VirtualFileNodeI vf' = case cast vf' of
    Just vf'' -> VirtualFileNodeI $ vf <> vf''
    Nothing -> error "Two differently typed VirtualFiles are at the same location"

-- | Information about the access just done, for logging purposes
data DataAccessDone = DidReadLoc String | DidWriteLoc String

-- | The internal part of a 'DataAccessNode, closing over the type params of the
-- access function
data DataAccessNodeI m where
  DataAccessNodeI :: (Typeable a, Typeable b) => (a -> m (b, [DataAccessDone])) -> DataAccessNodeI m

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

-- TODO: It is dubious that composing DataAccessNodes is really needed in the
-- end. Find a way to remove that.

instance Semigroup (VirtualFileOrData m f) where
  VirtualFileNodeE vf <> VirtualFileNodeE vf' = VirtualFileNodeE $ vf <> vf'
  DataAccessNodeE f <> DataAccessNodeE f' = DataAccessNodeE $ f <> f'
instance Monoid (VirtualFileOrData m WithDefaultUsage) where
  mempty = VirtualFileNodeE mempty
instance Monoid (VirtualFileOrData m LocLayers) where
  mempty = DataAccessNodeE mempty


-- pattern VirtualFileNode
--   :: (Typeable a, Typeable b, Monoid b) => PathlessVirtualFile a b -> VirtualFileNode m
pattern VirtualFileNode x = VirtualFileNodeE (Just (VirtualFileNodeI x))

-- pattern DataAccessNode
--   :: (Typeable a, Typeable b) => (a -> m b) -> DataAccessNode m
pattern DataAccessNode x = DataAccessNodeE (First (Just (DataAccessNodeI x)))
