{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module System.TaskPipeline.ResourceTree where

import Control.Lens
import Data.Typeable
import           Data.Locations.Mappings
import Data.Locations.LocationTree
import Data.Locations.VirtualFile
import           Data.Monoid                        (First (..))
import qualified Data.Text as T
import Data.List (intersperse)
import Data.Representable


-- * Resource tree nodes API

-- | The internal part of a 'VirtualFileNode', closing over the type params of
-- the 'VirtualFile'
data VirtualFileNodeI md where
  VirtualFileNodeI :: (Typeable a, Typeable b, Monoid b) => VirtualFile_ md a b -> VirtualFileNodeI md

instance (Semigroup md, Typeable md ) => Semigroup (VirtualFileNodeI md) where
  VirtualFileNodeI vf <> VirtualFileNodeI vf' = case cast vf' of
    Just vf'' -> VirtualFileNodeI $ vf <> vf''
    Nothing -> error "Two differently typed VirtualFiles are at the same location"

-- | Information about the access just done, for logging purposes
data DataAccessDone = DidReadLoc String | DidWriteLoc String

-- | The internal part of a 'DataAccessNode, closing over the type params of the
-- access function
data DataAccessNodeI m where
  DataAccessNodeI :: (Typeable a, Typeable b) => (a -> m (b, [DataAccessDone])) -> DataAccessNodeI m

-- These aliases are for compatibility with ATask. Will be removed in the future
-- when ATask is modified.
type InVirtualState = WithDefaultUsage
data InPhysicalState a
type InDataAccessState = LocLayers

-- | Each node of the 'ResourceTree' can be in 3 possible states
data ResourceTreeNode m state where
  VirtualFileNodeE
    :: Maybe (VirtualFileNodeI VFMetadata)
    -> ResourceTreeNode m InVirtualState  -- ^ State used when building the task pipeline
  PhysicalFileNodeE
    :: Maybe (VirtualFileNodeI (LocLayers FileExt, VFMetadata))
    -> ResourceTreeNode m InPhysicalState -- ^ State used for inspecting resource mappings
  DataAccessNodeE
    :: First (DataAccessNodeI m)
    -> ResourceTreeNode m InDataAccessState -- ^ State used when running the task pipeline

-- | The nodes of the LocationTree when using VirtualFiles
type VirtualFileNode m = ResourceTreeNode m InVirtualState
pattern VirtualFileNode x = VirtualFileNodeE (Just (VirtualFileNodeI x))

-- | The nodes of the LocationTree after the VirtualFiles have been resolved to
-- physical paths, and data possibly extracted from these paths
type DataAccessNode m = ResourceTreeNode m InDataAccessState
pattern DataAccessNode x = DataAccessNodeE (First (Just (DataAccessNodeI x)))

instance Show (ResourceTreeNode m InVirtualState) where
  show (VirtualFileNode vf) = show $ getVirtualFileDescription vf
  show _ = ""
  -- TODO: Cleaner Show
  -- TODO: Display read/written types here, since they're already Typeable

instance Show (ResourceTreeNode m InPhysicalState) where
  show (PhysicalFileNodeE (Just (VirtualFileNodeI vf))) = T.unpack
    (mconcat (intersperse " << "
             (map (toTextRepr . uncurry addExtToLocIfMissing') $
               toListOf (vfileStateData . _1 . locLayers) vf)))
    ++ " - " ++ show (getVirtualFileDescription vf)
  show _ = "null"

-- TODO: It is dubious that composing DataAccessNodes is really needed in the
-- end. Find a way to remove that.

instance Semigroup (ResourceTreeNode m st) where
  VirtualFileNodeE vf <> VirtualFileNodeE vf' = VirtualFileNodeE $ vf <> vf'
  DataAccessNodeE f <> DataAccessNodeE f' = DataAccessNodeE $ f <> f'
instance Monoid (ResourceTreeNode m InVirtualState) where
  mempty = VirtualFileNodeE mempty
instance Monoid (ResourceTreeNode m InDataAccessState) where
  mempty = DataAccessNodeE mempty


-- * Resource tree API

-- | The tree manipulated by tasks during their construction
type VirtualResourceTree m = LocationTree (VirtualFileNode m)

-- type PhysicalResourceTree = LocationTree 

-- | The tree manipulated by tasks when they actually run
type DataResourceTree m = LocationTree (DataAccessNode m)
