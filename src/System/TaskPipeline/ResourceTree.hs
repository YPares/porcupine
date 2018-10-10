{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This file describes the ResourceTree API. The ResourceTree is central to
-- every pipeline that runs. It is aggregated from each subtask composing the
-- pipeline, and can from that point exist in several states:
--
-- - Virtual: it contains only VirtualFiles. It is the state of the ResourceTree
--   that is used by the tasks to declare their requirements, and by the configuration
--   manager to write the default tree and mappings and read back the conf
-- - Physical: once configuration and mappings to physical files have been read,
--   each node is attached its corresponding physical locations. The locations for
--   the node which have no explicit mappings in the configuration are derived from the
--   nodes higher in the hierarchy (this implies that the only mandatory mapping is the
--   root of the tree). Physical resource trees are used to check that each virtual file
--   is compatible with the physical locations bound to it.
-- - DataAccess: once every location has been checked, we can replace the VirtualFiles in
--   the resource tree by the action that actually writes or reads the data from the
--   physical locations each VirtualFile has been bound to.
--
-- The VirtualFiles of a resource tree in the Virtual state, when written to the
-- configuration file, are divided into 2 sections: "data" and
-- "locations". "data" is a tree of the json objects corresponding to the
-- VirtualFiles that have default data which can be represented as 'Value's
-- (keeping the structure of the resource tree). "locations" contains a flat
-- json object of all the other nodes, mapped in a "/virtual/path:
-- physical_paths" fashion.
--
-- Indeed, each node can be mapped to _several_ physical paths, which we call
-- _layers_. We require everything that is read from a VirtualFile to be a
-- Monoid, so that we can combine the content of each layer into one value. This
-- Monoid should be right-biased (ie. in the expression @a <> b@, @b@ overwrites
-- the contents of @a@).
--
-- Some rules are applied when transitionning from Physical to DataAccess:
--
-- - if a physical location has an extension that is not recognized by the
--   VirtualFile it is bound to, the process fails
-- - if a VirtualFile has at the same time physical locations bound AND embedded data,
--   then the embedded data is considered to be the _rightmost_ layer (ie. the one
--   overriding all the other ones), so that the data of this VirtualFile can be
--   easily overriden by just changing the config file.

module System.TaskPipeline.ResourceTree where

import Control.Monad
import Control.Monad.Catch
import Control.Lens
import Data.Typeable
import           Data.Locations.SerializationMethod
import Data.Locations
import           Data.Monoid                        (First (..))
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.List (intersperse)
import Data.Representable
import Data.Aeson
import Data.Maybe
import Data.DocRecord
import Data.DocRecord.OptParse
import           System.TaskPipeline.CLI.Overriding
import Options.Applicative
import Katip


-- * API for manipulating resource tree _nodes_

-- | The internal part of a 'VirtualFileNode', closing over the type params of
-- the 'VirtualFile'
data SomeVirtualFile where
  SomeVirtualFile :: (Typeable a, Typeable b, Monoid b) => VirtualFile_ VFMetadata a b -> SomeVirtualFile

instance Semigroup SomeVirtualFile where
  SomeVirtualFile vf <> SomeVirtualFile vf' = case cast vf' of
    Just vf'' -> SomeVirtualFile $ vf <> vf''
    Nothing -> error "Two differently typed VirtualFiles are at the same location"

-- | The internal part of a 'DataAccessNode, closing over the type params of the
-- access function
data SomeDataAccess m where
  SomeDataAccess :: (Typeable a, Typeable b) => (a -> m b) -> SomeDataAccess m

-- These aliases are for compatibility with ATask. Will be removed in the future
-- when ATask is modified.
type InVirtualState = WithDefaultUsage
data InPhysicalState a
type InDataAccessState = LocLayers

-- | Each node of the 'ResourceTree' can be in 3 possible states
data ResourceTreeNode m state where
  VirtualFileNodeE
    :: Maybe SomeVirtualFile
    -> ResourceTreeNode m InVirtualState  -- ^ State used when building the task pipeline
  PhysicalFileNodeE
    :: Maybe (SomeVirtualFile, LocLayers (Maybe FileExt))
    -> ResourceTreeNode m InPhysicalState -- ^ State used for inspecting resource mappings
  DataAccessNodeE
    :: First (SomeDataAccess m)  -- Data access function isn't a semigroup,
                                 -- hence the use of First here instead of
                                 -- Maybe.
    -> ResourceTreeNode m InDataAccessState -- ^ State used when running the task pipeline

-- | The nodes of the LocationTree when using VirtualFiles
type VirtualFileNode m = ResourceTreeNode m InVirtualState
pattern VirtualFileNode x = VirtualFileNodeE (Just (SomeVirtualFile x))

type PhysicalFileNode m = ResourceTreeNode m InPhysicalState
pattern PhysicalFileNode x l = PhysicalFileNodeE (Just (SomeVirtualFile x, l))

-- | The nodes of the LocationTree after the VirtualFiles have been resolved to
-- physical paths, and data possibly extracted from these paths
type DataAccessNode m = ResourceTreeNode m InDataAccessState
pattern DataAccessNode x = DataAccessNodeE (First (Just (SomeDataAccess x)))

instance Semigroup (VirtualFileNode m) where
  VirtualFileNodeE vf <> VirtualFileNodeE vf' = VirtualFileNodeE $ vf <> vf'
instance Monoid (VirtualFileNode m) where
  mempty = VirtualFileNodeE mempty
-- TODO: It is dubious that composing DataAccessNodes is really needed in the
-- end. Find a way to remove that.
instance Semigroup (DataAccessNode m) where  
  DataAccessNodeE f <> DataAccessNodeE f' = DataAccessNodeE $ f <> f'
instance Monoid (DataAccessNode m) where
  mempty = DataAccessNodeE mempty

instance Show (VirtualFileNode m) where
  show (VirtualFileNode vf) = show $ getVirtualFileDescription vf
  show _ = ""
  -- TODO: Cleaner Show
  -- TODO: Display read/written types here, since they're already Typeable
instance Show (PhysicalFileNode m) where
  show (PhysicalFileNode vf layers) =
    T.unpack (mconcat
              (intersperse " << "
               (map locToText $
                 toListOf locLayers layers)))
    ++ " - " ++ show (getVirtualFileDescription vf)
    where
      locToText (loc, mbext) = toTextRepr $ addExtToLocIfMissing' loc (fromMaybe "" mbext)
  show _ = "null"


-- * API for manipulating resource trees globally

-- | The tree manipulated by tasks during their construction
type VirtualResourceTree m = LocationTree (VirtualFileNode m)

-- | The tree manipulated when checking if each location is bound to something
-- legit
type PhysicalResourceTree m = LocationTree (PhysicalFileNode m)

-- | The tree manipulated by tasks when they actually run
type DataResourceTree m = LocationTree (DataAccessNode m)

instance HasDefaultMappingRule (VirtualFileNode m) where
  isMappedByDefault (VirtualFileNode vf) = isMappedByDefault vf
  isMappedByDefault _ = True
                        -- Intermediary levels (folders, where there is no
                        -- VirtualFile) are kept

-- | Filters the tree to get only the nodes that don't have data and can be
-- mapped to external files
rscTreeToMappings
  :: VirtualResourceTree m
  -> Maybe (LocationMappings (VirtualFileNode m))
rscTreeToMappings tree = mappingsFromLocTree <$> over filteredLocsInTree rmOpts tree
  where
    rmOpts n@(VirtualFileNode vfile)
      | Just VFForCLIOptions <- intent = Nothing  -- Nodes with default data are
                                                  -- by default not put in the
                                                  -- mappings
      where intent = vfileDescIntent $ getVirtualFileDescription vfile
    rmOpts n = Just n

-- | Filters the tree to get only the nodes than can be embedded in the config file
rscTreeToEmbeddedDataTree
  :: VirtualResourceTree m
  -> Maybe (VirtualResourceTree m)
rscTreeToEmbeddedDataTree = over filteredLocsInTree keepOpts
  where
    keepOpts n@(VirtualFileNode vfile)
      | Just VFForCLIOptions <- intent = Just n
      | otherwise = Nothing
      where intent = vfileDescIntent $ getVirtualFileDescription vfile
    keepOpts n = Just n

embeddedDataSection :: T.Text
embeddedDataSection = "data"

mappingsSection :: T.Text
mappingsSection = "locations"

embeddedDataTreeToJSONFields
  :: T.Text -> VirtualResourceTree m -> [(T.Text, Value)]
embeddedDataTreeToJSONFields thisPath (LocationTree mbOpts sub) =
  [(thisPath, Object $ opts' <> sub')]
  where
    opts' = case mbOpts of
      VirtualFileNode vf -> case Right vf ^? vfileAsBidirE . vfileAesonValue of
        Just (Object o) -> o
      _ -> mempty
    sub' = HM.fromList $
      concat $ map (\(k,v) -> embeddedDataTreeToJSONFields (_ltpiName k) v) $ HM.toList sub


-- ** ResourceTreeAndMappings: join a virtual resource tree with the locations it
-- should be mapped to

-- | A 'VirtualResourceTree' associated with the mapping that should be applied
-- to it. This is the way to serialize and deserialize a resource tree
data ResourceTreeAndMappings m =
  ResourceTreeAndMappings (VirtualResourceTree m)
                          (Either Loc (LocationMappings FileExt))

-- ResourceTreeAndMappings is only 'ToJSON' and not 'FromJSON' because we need
-- more context to deserialize it. It is done by rscTreeConfigurationReader
instance ToJSON (ResourceTreeAndMappings m) where
  toJSON (ResourceTreeAndMappings tree mappings) = Object $
    (case rscTreeToMappings tree of
       Just m ->
         HM.singleton mappingsSection $ toJSON' $ case mappings of
           Right m'     -> m'
           Left rootLoc -> mappingRootOnly rootLoc (Just "") <> fmap nodeExt m)
    <>
    (case rscTreeToEmbeddedDataTree tree of
      Just t  -> HM.fromList $ embeddedDataTreeToJSONFields embeddedDataSection t
      Nothing -> HM.empty)
    where
      toJSON' :: LocationMappings FileExt -> Value
      toJSON' = toJSON
      nodeExt :: MbLocWithExt (VirtualFileNode m) -> MbLocWithExt FileExt
      nodeExt (MbLocWithExt loc (Just (VirtualFileNode
                                       (_serialDefaultExt . _vfileSerials -> First ext)))) =
        MbLocWithExt loc ext
      nodeExt (MbLocWithExt loc _) = MbLocWithExt loc Nothing

-- ** Reading virtual resource trees from the input

-- | Reads the data from the input config file. Constructs the parser for the
-- command-line arguments. Combines both results to create the
-- 'VirtualResourceTree' (and its mappings) the pipeline should run on.
rscTreeConfigurationReader
  :: forall m. VirtualResourceTree m
  -> CLIOverriding (ResourceTreeAndMappings m)
                   (LocationTree (VirtualFileNode m, Maybe (RecOfOptions SourcedDocField)))
rscTreeConfigurationReader defTree =
  CLIOverriding overridesParser_ nullOverrides_ overrideCfgFromYamlFile_
  where
    overridesParser_ = traverseOf (traversed . _2 . _Just) parseOptions $
                       fmap nodeAndRecOfOptions defTree
    nodeAndRecOfOptions :: VirtualFileNode m -> (VirtualFileNode m, Maybe DocRecOfOptions)
    nodeAndRecOfOptions n@(VirtualFileNode vf) = (n, vf ^? vfileAsBidir . vfileRecOfOptions)
    nodeAndRecOfOptions n = (n, Nothing)
    parseOptions :: RecOfOptions DocField -> Parser (RecOfOptions SourcedDocField)
    parseOptions (RecOfOptions r) = RecOfOptions <$>
      parseRecFromCLI (tagWithDefaultSource r)

    nullOverrides_ = allOf (traversed . _2 . _Just) nullRec
    nullRec (RecOfOptions RNil) = True
    nullRec _ = False
    
    overrideCfgFromYamlFile_ aesonCfg optsTree =
      ([], ResourceTreeAndMappings
           <$> traverseOf traversedTreeWithPath (integrateAesonCfg aesonCfg) optsTree
           <*> case aesonCfg of
                 Object (HM.lookup mappingsSection -> Just m) ->
                   Right <$> parseJSONEither m  -- mappings is an Either field
                                                -- in ResourceTreeAndMappings
                 _ -> Left $
                   "Section '" ++ T.unpack mappingsSection
                   ++ "' not found in the config file")

    integrateAesonCfg
      :: Value
      -> (LocationTreePath, (VirtualFileNode m, Maybe (RecOfOptions SourcedDocField)))
      -> Either String (VirtualFileNode m)
    integrateAesonCfg aesonCfg (LTP path, (node@(VirtualFileNode vf), mbRecFromCLI)) =
      let mbAesonValInCfg = findInAesonVal (LTPI embeddedDataSection : path) aesonCfg
      in case mbAesonValInCfg of
          Right v -> case mbRecFromCLI of
            Just (RecOfOptions recFromCLI) -> do
              -- YAML: yes, CLI: yes
              recFromYaml <- tagWithYamlSource <$> parseJSONEither v
                -- We merge the two configurations:
              let newOpts = RecOfOptions $ rmTags $ rzipWith chooseHighestPriority recFromYaml recFromCLI
              return $ VirtualFileNode $ vf & vfileAsBidir . vfileRecOfOptions .~ newOpts
            Nothing ->
              -- YAML: yes, CLI: no
              VirtualFileNode <$> (Right vf & vfileAsBidirE . vfileAesonValue .~ v)
          Left _ -> case mbRecFromCLI of
            Just (RecOfOptions recFromCLI) ->
              -- YAML: no, CLI: yes
              return $ VirtualFileNode $
                vf & vfileAsBidir . vfileRecOfOptions .~ RecOfOptions (rmTags recFromCLI)
            Nothing ->
              -- YAML: no, CLI: no
              return node
    integrateAesonCfg _ (_, (node, _)) = return node
    
    findInAesonVal path v = go path v
      where
        go [] v = return v
        go (p:ps) (Object (HM.lookup (_ltpiName p) -> Just v)) = go ps v
        go _ _ = Left $ "rscTreeConfigurationReader: " ++
          (T.unpack $ toTextRepr $ LTP path) ++ " doesn't match any path in the Yaml config"

-- ** Transforming a virtual resource tree to a physical resource tree (ie. a
-- tree with physical locations attached)

-- | Transform a virtual file node in file node with physical locations
applyOneRscMapping :: Maybe (LocLayers (Maybe FileExt)) -> VirtualFileNode m -> PhysicalFileNode m
applyOneRscMapping (Just layers) (VirtualFileNode vf) = PhysicalFileNode vf layers
applyOneRscMapping _ _ = PhysicalFileNodeE Nothing

-- | Binding a 'VirtualResourceTree'
applyMappingsToResourceTree :: ResourceTreeAndMappings m -> PhysicalResourceTree m
applyMappingsToResourceTree (ResourceTreeAndMappings tree mappings) =
  applyMappings applyOneRscMapping m' tree
  where
    m' = case mappings of
           Right m -> m
           Left rootLoc -> mappingRootOnly rootLoc Nothing

-- ** Transforming a physical resource tree to a data access tree (ie. a tree
-- where each node is just a function that pulls or writes the relevant data)

data TaskConstructionError =
  TaskConstructionError String
  deriving (Show)
instance Exception TaskConstructionError

-- | Transform a file node with physical locations in node with a data access
-- function to run. Matches the location (esp. file extensions) to writers
-- available in the 'VirtualFile'.
resolveDataAccess
  :: forall m m'. (LocationMonad m, KatipContext m, MonadThrow m')
  => PhysicalFileNode m -> m' (DataAccessNode m)
resolveDataAccess (PhysicalFileNode vf (toListOf locLayers -> layers)) = do
  writeLocs <- findFunctions writers
  readLocs <- findFunctions readers
  return $ DataAccessNode $ \input -> do
    forM_ writeLocs $ \(WriteToLocFn f, loc) -> do      
      f input loc
      logFM InfoS $ logStr $ "Successfully wrote file '" ++ show loc ++ "'"
    layersRes <- mconcat <$> forM readLocs(\(ReadFromLocFn f, loc) -> do
      r <- f loc
      logFM InfoS $ logStr $ "Successfully read file '" ++ show loc ++ "'"
      return r)
    return $ case vf ^? vfileEmbeddedValue of
      Just v -> layersRes <> v
      Nothing -> layersRes
  where
    readers = vf ^. vfileSerials . serialReaders . serialReadersFromInputFile
    writers = vf ^. vfileSerials . serialWriters . serialWritersToOutputFile
    findFunctions :: HM.HashMap FileExt v -> m' [(v, Loc)]
    findFunctions hm | HM.null hm = return []
                     | otherwise  = mapM findFunction layers
      where
        findFunction (loc, fromMaybe "" -> ext) = case HM.lookup ext hm of
          Just f -> return (f, loc)
          -- TODO: add VirtualFile path to error
          Nothing -> throwM $ TaskConstructionError $
            "Extension '" ++ T.unpack ext ++ "' insn't supported by the VirtualFile which "
            ++ show loc ++ " is bound to. Accepted extensions are " ++ show (HM.keys hm) ++ "."
resolveDataAccess _ = return $ DataAccessNodeE $ First Nothing
