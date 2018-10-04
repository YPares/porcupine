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


-- * API for manipulating resource tree _nodes_

-- | The internal part of a 'VirtualFileNode', closing over the type params of
-- the 'VirtualFile'
data SomeVirtualFile md where
  SomeVirtualFile :: (Typeable a, Typeable b, Monoid b) => VirtualFile_ md a b -> SomeVirtualFile md

instance (Semigroup md, Typeable md ) => Semigroup (SomeVirtualFile md) where
  SomeVirtualFile vf <> SomeVirtualFile vf' = case cast vf' of
    Just vf'' -> SomeVirtualFile $ vf <> vf''
    Nothing -> error "Two differently typed VirtualFiles are at the same location"

-- | Information about the access just done, for logging purposes
data DataAccessDone = DidReadLoc String | DidWriteLoc String

-- | The internal part of a 'DataAccessNode, closing over the type params of the
-- access function
data SomeDataAccess m where
  SomeDataAccess :: (Typeable a, Typeable b) => (a -> m (b, [DataAccessDone])) -> SomeDataAccess m

-- These aliases are for compatibility with ATask. Will be removed in the future
-- when ATask is modified.
type InVirtualState = WithDefaultUsage
data InPhysicalState a
type InDataAccessState = LocLayers

-- | Each node of the 'ResourceTree' can be in 3 possible states
data ResourceTreeNode m state where
  VirtualFileNodeE
    :: Maybe (SomeVirtualFile VFMetadata)
    -> ResourceTreeNode m InVirtualState  -- ^ State used when building the task pipeline
  PhysicalFileNodeE
    :: Maybe (SomeVirtualFile (LocLayers FileExt, VFMetadata))
    -> ResourceTreeNode m InPhysicalState -- ^ State used for inspecting resource mappings
  DataAccessNodeE
    :: First (SomeDataAccess m)
    -> ResourceTreeNode m InDataAccessState -- ^ State used when running the task pipeline

-- | The nodes of the LocationTree when using VirtualFiles
type VirtualFileNode m = ResourceTreeNode m InVirtualState
pattern VirtualFileNode x = VirtualFileNodeE (Just (SomeVirtualFile x))

-- | The nodes of the LocationTree after the VirtualFiles have been resolved to
-- physical paths, and data possibly extracted from these paths
type DataAccessNode m = ResourceTreeNode m InDataAccessState
pattern DataAccessNode x = DataAccessNodeE (First (Just (SomeDataAccess x)))

-- TODO: It is dubious that composing DataAccessNodes is really needed in the
-- end. Find a way to remove that.
instance Semigroup (ResourceTreeNode m st) where
  VirtualFileNodeE vf <> VirtualFileNodeE vf' = VirtualFileNodeE $ vf <> vf'
  DataAccessNodeE f <> DataAccessNodeE f' = DataAccessNodeE $ f <> f'
instance Monoid (ResourceTreeNode m InVirtualState) where
  mempty = VirtualFileNodeE mempty
instance Monoid (ResourceTreeNode m InDataAccessState) where
  mempty = DataAccessNodeE mempty

instance Show (ResourceTreeNode m InVirtualState) where
  show (VirtualFileNode vf) = show $ getVirtualFileDescription vf
  show _ = ""
  -- TODO: Cleaner Show
  -- TODO: Display read/written types here, since they're already Typeable
instance Show (ResourceTreeNode m InPhysicalState) where
  show (PhysicalFileNodeE (Just (SomeVirtualFile vf))) =
    T.unpack (mconcat
              (intersperse " << "
               (map (toTextRepr . uncurry addExtToLocIfMissing') $
                 toListOf (vfileStateData . _1 . locLayers) vf)))
    ++ " - " ++ show (getVirtualFileDescription vf)
  show _ = "null"


-- * API for manipulating resource trees globally

-- | The tree manipulated by tasks during their construction
type VirtualResourceTree m = LocationTree (VirtualFileNode m)

-- type PhysicalResourceTree = LocationTree 

-- | The tree manipulated by tasks when they actually run
type DataResourceTree m = LocationTree (DataAccessNode m)

instance HasDefaultMappingRule (ResourceTreeNode m InVirtualState) where
  isMappedByDefault (VirtualFileNode vf) = isMappedByDefault vf
  isMappedByDefault _ = True
                        -- Intermediary levels (folders, where there is no
                        -- VirtualFile) are kept

rscTreeToMappings :: VirtualResourceTree m -> Maybe (LocationMappings (VirtualFileNode m))
rscTreeToMappings tree = mappingsFromLocTree <$> over filteredLocsInTree rmOpts tree
  where
    rmOpts n@(VirtualFileNode vfile) = case intent of
      Just VFForCLIOptions -> Nothing  -- options shouldn't appear in the
                                       -- resource tree
      _ -> Just n
      where
        intent = vfileDescIntent $ getVirtualFileDescription vfile
    rmOpts n = Just n

-- rscTreeToOptionTree :: VirtualResourceTree m -> Maybe (LocationTree (Maybe DocRecOfOptions))
-- rscTreeToOptionTree = over filteredLocsInTree
