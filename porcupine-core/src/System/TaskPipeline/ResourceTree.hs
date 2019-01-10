{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

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

import           Control.Exception.Safe
import           Control.Lens                            hiding ((:>), (<.>))
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString                         as Strict
import qualified Data.ByteString.Lazy                    as Lazy
import qualified Data.ByteString.Streaming               as BSS
import           Data.DocRecord
import           Data.DocRecord.OptParse
import qualified Data.HashMap.Strict                     as HM
import           Data.List                               (intersperse)
import           Data.Locations                          hiding (readBSS,
                                                          writeBSS)
import           Data.Locations.Accessors
import           Data.Maybe
import           Data.Monoid                             (First (..))
import           Data.Representable
import qualified Data.Text                               as T
import qualified Data.Text.Lazy                          as LT
import qualified Data.Text.Lazy.Encoding                 as LTE
import           Data.Typeable
import           GHC.Generics                            (Generic)
import           Katip
import           Options.Applicative
import           Streaming
import           System.TaskPipeline.ConfigurationReader


-- * API for manipulating resource tree _nodes_

-- | The internal part of a 'VirtualFileNode', closing over the type params of
-- the 'VirtualFile'
data SomeVirtualFile where
  SomeVirtualFile :: (Typeable a, Typeable b)
                  => VirtualFile a b
                  -> SomeVirtualFile

instance Semigroup SomeVirtualFile where
  SomeVirtualFile vf <> SomeVirtualFile vf' = case cast vf' of
    Just vf'' -> SomeVirtualFile (vf <> vf'')
    Nothing -> error "Two differently typed VirtualFiles are at the same location"

-- | Packs together the two functions that will read
data DataAccessor m a b = DataAccessor
  { daPerformWrite :: a -> m ()
  , daPerformRead  :: m b
  , daLocsAccessed :: Either String [SomeLoc m] }

-- | The internal part of a 'DataAccessNode, closing over the type params of the
-- access function.
data SomeDataAccess m where
  SomeDataAccess :: (Typeable a, Typeable b)
                 => (LocVariableMap -> DataAccessor m a b) -> SomeDataAccess m

-- | Tells the type of accesses that some VirtualFile will undergo. They are
-- accumulated though the whole pipeline.
data VFNodeAccessType = ATWrite | ATRead
  deriving (Eq, Show)

-- | The nodes of the ResourceTree, before mapping each 'VirtualFiles' to
-- physical locations
data VirtualFileNode = MbVirtualFileNode [VFNodeAccessType] (Maybe SomeVirtualFile)
-- | A non-empty 'VirtualFileNode'
pattern VirtualFileNode {vfnodeAccesses, vfnodeFile} =
  MbVirtualFileNode vfnodeAccesses (Just (SomeVirtualFile vfnodeFile))

-- | vfnodeFile is a @Traversal'@ into the VirtualFile contained in a
-- VirtualFileNode, but hiding it's real read/write types.
vfnodeFileVoided :: Traversal' VirtualFileNode (VirtualFile Void ())
vfnodeFileVoided f (VirtualFileNode at vf) =
  VirtualFileNode at <$> vfileVoided f vf
vfnodeFileVoided _ vfn = pure vfn

-- | The nodes of the ResourceTree, after mapping each 'VirtualFiles' to
-- physical locations
data PhysicalFileNode m = MbPhysicalFileNode [SomeLocWithVars m] (Maybe SomeVirtualFile)
-- | A non-empty 'PhysicalFileNode'
pattern PhysicalFileNode l vf = MbPhysicalFileNode l (Just (SomeVirtualFile vf))

-- | The nodes of the LocationTree after the 'VirtualFiles' have been resolved
-- to physical paths, and data possibly extracted from these paths
data DataAccessNode m = MbDataAccessNode [SomeLocWithVars m] (First (SomeDataAccess m))
  -- Data access function isn't a semigroup, hence the use of First here instead
  -- of Maybe.
-- | A non-empty 'DataAccessNode'
pattern DataAccessNode l x = MbDataAccessNode l (First (Just (SomeDataAccess x)))


instance Semigroup VirtualFileNode where
  MbVirtualFileNode ats vf <> MbVirtualFileNode ats' vf' =
    MbVirtualFileNode (ats <> ats') (vf <> vf')
instance Monoid VirtualFileNode where
  mempty = MbVirtualFileNode [] mempty
-- TODO: It is dubious that composing DataAccessNodes is really needed in the
-- end. Find a way to remove that.
instance Semigroup (DataAccessNode m) where
  MbDataAccessNode locs f <> MbDataAccessNode _ f' = MbDataAccessNode locs $ f <> f'
instance Monoid (DataAccessNode m) where
  mempty = MbDataAccessNode [] mempty

instance Show VirtualFileNode where
  show (VirtualFileNode{..}) =
    "VirtualFileNode with " ++ show (getVirtualFileDescription vfnodeFile)
    ++ " accessed for: " ++ show vfnodeAccesses
  show _                    = ""
  -- TODO: Cleaner Show
  -- TODO: Display read/written types here, since they're already Typeable

toJSONTxt :: SomeLocWithVars m -> T.Text
toJSONTxt (SomeGLoc a) = case toJSON a of
  String s -> s
  v        -> LT.toStrict $ LTE.decodeUtf8 $ encode v

instance Show (PhysicalFileNode m) where
  show (MbPhysicalFileNode layers mbVF) =
    T.unpack (mconcat
              (intersperse " << "
               (map toJSONTxt layers)))
    ++ case mbVF of
         Just (SomeVirtualFile vf) ->
           " - " ++ show (getVirtualFileDescription vf)
         _ -> ""


-- * API for manipulating resource trees globally

-- | The tree manipulated by tasks during their construction
type VirtualResourceTree = LocationTree VirtualFileNode

-- | The tree manipulated when checking if each location is bound to something
-- legit
type PhysicalResourceTree m = LocationTree (PhysicalFileNode m)

-- | The tree manipulated by tasks when they actually run
type DataResourceTree m = LocationTree (DataAccessNode m)

instance HasDefaultMappingRule VirtualFileNode where
  getDefaultLocShortcut (VirtualFileNode{..}) = getDefaultLocShortcut vfnodeFile
  getDefaultLocShortcut _                     = Nothing

-- | Filters the tree to get only the nodes that don't have data and can be
-- mapped to external files
rscTreeToMappings
  :: VirtualResourceTree
  -> Maybe LocationMappings
rscTreeToMappings tree = mappingsFromLocTree <$> over filteredLocsInTree rmOpts tree
  where
    rmOpts (VirtualFileNode{..})
      | Just VFForCLIOptions <- intent = Nothing  -- Nodes with default data are
                                                  -- by default not put in the
                                                  -- mappings
      where intent = vfileDescIntent $ getVirtualFileDescription vfnodeFile
    rmOpts n = Just n

-- | Filters the tree to get only the nodes than can be embedded in the config file
rscTreeToEmbeddedDataTree
  :: VirtualResourceTree
  -> Maybe VirtualResourceTree
rscTreeToEmbeddedDataTree = over filteredLocsInTree keepOpts
  where
    keepOpts n@(VirtualFileNode{..})
      | Just VFForCLIOptions <- intent = Just n
      | otherwise = Nothing
      where intent = vfileDescIntent $ getVirtualFileDescription vfnodeFile
    keepOpts n = Just n

variablesSection :: T.Text
variablesSection = "variables"

embeddedDataSection :: T.Text
embeddedDataSection = "data"

mappingsSection :: T.Text
mappingsSection = "locations"

embeddedDataTreeToJSONFields
  :: T.Text -> VirtualResourceTree -> [(T.Text, Value)]
embeddedDataTreeToJSONFields thisPath (LocationTree mbOpts sub) =
  [(thisPath, Object $ opts' <> sub')]
  where
    opts' = case mbOpts of
      VirtualFileNode{..} ->
        case Right vfnodeFile ^? vfileAsBidirE . vfileAesonValue of
          Just (Object o) -> o
          _               -> mempty
      _ -> mempty
    sub' = HM.fromList $
      concat $ map (\(k,v) -> embeddedDataTreeToJSONFields (_ltpiName k) v) $ HM.toList sub


-- ** ResourceTreeAndMappings: join a virtual resource tree with the locations it
-- should be mapped to

-- | A 'VirtualResourceTree' associated with the mapping that should be applied
-- to it. This is the way to serialize and deserialize a resource tree
data ResourceTreeAndMappings = ResourceTreeAndMappings
  { rtamResourceTree :: VirtualResourceTree
  , rtamMappings     :: Either Loc LocationMappings
  , rtamVariables    :: LocVariableMap }

-- ResourceTreeAndMappings is only 'ToJSON' and not 'FromJSON' because we need
-- more context to deserialize it. It is done by rscTreeConfigurationReader
instance ToJSON ResourceTreeAndMappings where
  toJSON (ResourceTreeAndMappings tree mappings variables) = Object $
    (case rscTreeToEmbeddedDataTree tree of
      Just t  -> HM.fromList $ embeddedDataTreeToJSONFields embeddedDataSection t
      Nothing -> HM.empty)
    <>
    (case rscTreeToMappings tree of
       Just m ->
         HM.singleton mappingsSection $ toJSON $ case mappings of
           Right m'     -> m'
           Left rootLoc -> mappingRootOnly rootLoc <> m
       Nothing -> HM.empty)
    <>
    (HM.singleton variablesSection $ toJSON variables)

-- ** Reading virtual resource trees from the input

data LayerOperator = ReplaceLayers | AddLayer

type ResourceTreeAndMappingsOverrides =
  ( LocVariableMap
    -- The map of variables and their values read from CLI too
  , [(LocationTreePath, LayerOperator, SerializableLocShortcut)]
    -- Locations mapped to new layers
  , LocationTree (VirtualFileNode, Maybe (RecOfOptions SourcedDocField))
    -- The tree containing the options parsed by optparse-applicative
  )

-- | Reads the data from the input config file. Constructs the parser for the
-- command-line arguments. Combines both results to create the
-- 'VirtualResourceTree' (and its mappings) the pipeline should run on.
rscTreeConfigurationReader
  :: ResourceTreeAndMappings
  -> ConfigurationReader ResourceTreeAndMappings ResourceTreeAndMappingsOverrides
rscTreeConfigurationReader (ResourceTreeAndMappings{rtamResourceTree=defTree}) =
  ConfigurationReader overridesParser_ nullOverrides_ overrideCfgFromYamlFile_
  where
    overridesParser_ =
      (,,) <$> variablesParser <*> mappingsParser <*> treeOfOptsParser
      where
        treeOfOptsParser = traverseOf (traversed . _2 . _Just) parseOptions $
                           fmap nodeAndRecOfOptions defTree
        variablesParser = HM.fromList <$>
          many (option (eitherReader varBinding)
                 (long "var"
               <> help "Set a variable already present in the config file"))
        varBinding (T.splitOn "=" . T.pack -> [T.unpack -> var, T.unpack -> val]) =
          Right (LocVariable var,val)
        varBinding _ = Left "Var binding must be of the form \"variable=value\""
        mappingsParser =
          many (option (eitherReader locBinding)
                 (long "loc"
               <> help "Map a virtual file path to a physical location"))
        parseLocBinding vpath locOp loc = do
          p <- fromTextRepr vpath
          l <- parseJSONEither $ String loc
          return (p,locOp,l)
        locBinding (T.splitOn "=" . T.pack -> [vpath,loc]) =
          parseLocBinding vpath ReplaceLayers loc
        locBinding (T.splitOn "+=" . T.pack -> [vpath,loc]) =
          parseLocBinding vpath AddLayer loc
        locBinding _ =
          Left "Location mapping must be of the form \"virtual_path(+)=physical_path\""

    nodeAndRecOfOptions :: VirtualFileNode -> (VirtualFileNode, Maybe DocRecOfOptions)
    nodeAndRecOfOptions n@(VirtualFileNode{..}) = (n, vfnodeFile ^? vfileAsBidir . vfileRecOfOptions)
    nodeAndRecOfOptions n = (n, Nothing)
    parseOptions :: RecOfOptions DocField -> Parser (RecOfOptions SourcedDocField)
    parseOptions (RecOfOptions r) = RecOfOptions <$>
      parseRecFromCLI (tagWithDefaultSource r)

    nullOverrides_ (_,_,t) = allOf (traversed . _2 . _Just) nullRec t
    nullRec (RecOfOptions RNil) = True
    nullRec _                   = False

    overrideCfgFromYamlFile_ (Object aesonCfg) (cliVars, cliMappings, embeddedDataTree) = ([], rtam)
      where
        dataSectionContent = HM.lookup embeddedDataSection aesonCfg
        mappingsSectionContent = HM.lookup mappingsSection aesonCfg
        variablesSectionContent = HM.lookup variablesSection aesonCfg
        addCLIMappings (LocationMappings_ yamlMappings) =
          LocationMappings_ $ foldl addOne yamlMappings cliMappings
          where
            addOne mappings (path, locOp, loc) = HM.alter (go locOp loc) path mappings
            go AddLayer loc (Just locs) = Just $ locs ++ [loc]
            go _        loc _           = Just [loc]
        rtam = ResourceTreeAndMappings
          <$> traverseOf traversedTreeWithPath
                (replaceWithDataFromConfig dataSectionContent) embeddedDataTree
          <*> (Right . addCLIMappings <$> case mappingsSectionContent of
                 Just m -> parseJSONEither m
                 _      -> pure mempty)
          <*> ((cliVars <>) <$> case variablesSectionContent of
                 Just m -> parseJSONEither m
                 _      -> pure mempty)
    overrideCfgFromYamlFile_ _ _ = ([], Left "Configuration file doesn't contain a JSON object")

    replaceWithDataFromConfig
      :: Maybe Value  -- The content of the embedded data section
      -> (LocationTreePath, (VirtualFileNode, Maybe (RecOfOptions SourcedDocField)))
      -> Either String VirtualFileNode
    replaceWithDataFromConfig (Just dataSectionContent)
                              (LTP path, (node@(VirtualFileNode{..}), mbRecFromCLI)) =
      let rebuildNode newF = VirtualFileNode{vfnodeFile = newF, ..}
      in case findInAesonVal path dataSectionContent of
          Right v -> case mbRecFromCLI of
            Just (RecOfOptions recFromCLI) -> do
              -- YAML: yes, CLI: yes
              recFromYaml <- tagWithYamlSource <$> parseJSONEither v
                -- We merge the two configurations:
              let newOpts = RecOfOptions $ rmTags $
                    rzipWith chooseHighestPriority recFromYaml recFromCLI
              return $ rebuildNode $ vfnodeFile & vfileAsBidir . vfileRecOfOptions .~ newOpts
            Nothing ->
              -- YAML: yes, CLI: no
              rebuildNode <$> (Right vfnodeFile & vfileAsBidirE . vfileAesonValue .~ v)
          Left _ -> case mbRecFromCLI of
            Just (RecOfOptions recFromCLI) ->
              -- YAML: no, CLI: yes
              return $ rebuildNode $
                vfnodeFile & vfileAsBidir . vfileRecOfOptions .~ RecOfOptions (rmTags recFromCLI)
            Nothing ->
              -- YAML: no, CLI: no
              return node
    replaceWithDataFromConfig _ (_, (node, _)) = return node

    findInAesonVal path = go path
      where
        go [] v = return v
        go (p:ps) (Object (HM.lookup (_ltpiName p) -> Just v)) = go ps v
        go _ _ = Left $ "rscTreeConfigurationReader: " ++
          (T.unpack $ toTextRepr $ LTP path) ++ " doesn't match any path in the Yaml config"

-- ** Transforming a virtual resource tree to a physical resource tree (ie. a
-- tree with physical locations attached)

-- | Transform a virtual file node in file node with definite physical
-- locations. Splices in the locs the variables that can be spliced.
applyOneRscMapping :: LocVariableMap -> [SomeLocWithVars m] -> VirtualFileNode -> Bool -> PhysicalFileNode m
applyOneRscMapping variables configLayers mbVF mappingIsExplicit = buildPhysicalNode mbVF
  where
    configLayers' = map (\(SomeGLoc l) -> SomeGLoc $ spliceLocVariables variables l) configLayers
    buildPhysicalNode (VirtualFileNode{..}) = PhysicalFileNode layers vfnodeFile
      where
        First defExt = vfnodeFile ^. vfileSerials . serialDefaultExt
        intent = vfileDescIntent $ getVirtualFileDescription vfnodeFile
        layers | not mappingIsExplicit, Just VFForCLIOptions <- intent = []
             -- Options usually present in the config file need an _explicit_
             -- mapping to be present in the config file, if we want them to be
             -- read from external files instead
               | otherwise = map resolveExt configLayers'
        resolveExt (SomeGLoc loc) = SomeGLoc $ setLocTypeIfMissing loc $ T.unpack $ fromMaybe "" defExt
    buildPhysicalNode _ = MbPhysicalFileNode configLayers' Nothing

-- | Binds together a 'VirtualResourceTree' with physical locations an splices
-- in the variables read from the configuration.
getPhysicalResourceTreeFromMappings
  :: (LogThrow m) => ResourceTreeAndMappings -> LocResolutionM m (PhysicalResourceTree m)
getPhysicalResourceTreeFromMappings (ResourceTreeAndMappings tree mappings variables) =
  applyMappings (applyOneRscMapping variables) m' tree
  where
    m' = case mappings of
           Right m      -> m
           Left rootLoc -> mappingRootOnly rootLoc

-- ** Transforming a physical resource tree to a data access tree (ie. a tree
-- where each node is just a function that pulls or writes the relevant data)

data TaskConstructionError =
  TaskConstructionError String
  deriving (Show)
instance Exception TaskConstructionError

data DataAccessContext = DAC
  { locationAccessed     :: Value
  , requiredLocVariables :: [LocVariable]
  , providedLocVariables :: LocVariableMap
  , splicedLocation      :: Value }
  deriving (Generic)

instance ToJSON DataAccessContext
instance ToObject DataAccessContext
instance LogItem DataAccessContext where
  payloadKeys v _
    | v == V3   = AllKeys
    | v >= V1   = SomeKeys ["locationAccessed"]
    | otherwise = SomeKeys []

makeDataAccessor
  :: forall m a b. (LogMask m)
  => String  -- ^ VirtualFile path (for doc)
  -> VFileImportance  -- ^ How to log the accesses
  -> [SomeLocWithVars m]  -- ^ Every mapped layer (for doc)
  -> Maybe b -- ^ Default value (used as base layer)
  -> LayeredReadScheme b  -- ^ How to handle the different layers
  -> [(ToAtomicFn a, SomeLocWithVars m)]  -- ^ Layers to write to
  -> [(FromStreamFn b, SomeLocWithVars m)] -- ^ Layers to read from
  -> LocVariableMap  -- ^ The map of the values of the repetition indices
  -> DataAccessor m a b
makeDataAccessor vpath (VFileImportance sevRead sevWrite sevError)
                 layers mbDefVal readScheme writeLocs readLocs repetKeyMap =
  DataAccessor{..}
  where
    daLocsAccessed = traverse (\(SomeGLoc loc) -> SomeGLoc <$> fillLoc' repetKeyMap loc) layers
    daPerformWrite input = do
        forM_ writeLocs $ \(ToAtomicFn {-rkeys-} f, SomeGLoc loc) ->
          case cast (f input) of
            Nothing -> error "Some atomic serializer isn't converting to a lazy ByteString"
            Just bs -> do
              loc' <- fillLoc repetKeyMap loc
              katipAddNamespace "dataAccessor" $ katipAddNamespace "writer" $
                katipAddContext (DAC (toJSON loc) {-rkeys-}mempty repetKeyMap (toJSON loc')) $ do
                  let runWrite = writeBSS loc' (BSS.fromLazy bs)
                  withException runWrite $ \ioError ->
                    logFM sevError $ logStr $ displayException (ioError :: IOException)
                  logFM sevWrite $ logStr $ "Wrote '" ++ show loc' ++ "'"
    daPerformRead = do
        dataFromLayers <- forM readLocs (\(FromStreamFn {-rkeys-} (f :: Stream (Of i) m () -> m (Of b ())), SomeGLoc loc) ->
          case eqT :: Maybe (i :~: Strict.ByteString) of
            Nothing -> error "Some stream reader isn't expecting a stream of strict ByteStrings"
            Just Refl -> do
              loc' <- fillLoc repetKeyMap loc
              katipAddNamespace "dataAccessor" $ katipAddNamespace "reader" $
                katipAddContext (DAC (toJSON loc) {-rkeys-}mempty repetKeyMap (toJSON loc')) $ do
                  let runRead = readBSS loc' (f . BSS.toChunks)
                  (r :> ()) <- withException runRead $ \ioError ->
                    logFM sevError $ logStr $ displayException (ioError :: IOException)
                  logFM sevRead $ logStr $ "Read '" ++ show loc' ++ "'"
                  return r)
        let embeddedValAndLayers = maybe id (:) mbDefVal dataFromLayers
        case (readScheme, embeddedValAndLayers) of
          (_, [x]) -> return x
          (LayeredReadWithNull, ls) -> return $ mconcat ls
          (_, []) -> throwWithPrefix $ vpath ++ " has no layers from which to read"
          (LayeredRead, l:ls) -> return $ foldr (<>) l ls
          (SingleLayerRead, ls) -> do
            logFM WarningS $ logStr $ vpath ++
              " doesn't support layered mapping. Using only result from last layer '"
              ++ show (last $ layers) ++ "'"
            return $ last ls

    fillLoc' rkMap loc = terminateLocWithVars $ spliceLocVariables rkMap loc
    fillLoc rkMap loc =
      case fillLoc' rkMap loc of
        Left e  -> katipAddNamespace "dataAccessor" $ throwWithPrefix e
        Right r -> return r

-- | Transform a file node with physical locations in node with a data access
-- function to run. Matches the location (especially the filetype/extension) to
-- the readers & writers available in the 'VirtualFile'.
resolveDataAccess
  :: forall m m'. (LogMask m, MonadThrow m')
  => PhysicalFileNode m
  -> m' (DataAccessNode m)
resolveDataAccess (PhysicalFileNode layers vf) = do
  -- resolveDataAccess performs some checks when we build the pipeline: --
  -- First, that we aren't illegally binding to no layers:
  case layers of
    [] -> case readScheme of
            LayeredReadWithNull -> return ()
            _ -> case mbEmbeddedVal of
              Just _ -> return ()
              Nothing ->
                throwM $ TaskConstructionError $
                vpath ++ " cannot be mapped to null. It doesn't contain any default value."
    _ -> return ()
  -- Then, that we aren't writing to an unsupported filetype:
  writeLocs <- findFunctions (typeOf (undefined :: Lazy.ByteString)) writers
  -- And finally, that we aren't reading from an unsupported filetype:
  readLocs <- findFunctions (typeOf (undefined :: Strict.ByteString)) readers
  return $
    DataAccessNode layers $
      makeDataAccessor vpath (vf^.vfileImportance) layers
                       mbEmbeddedVal readScheme
                       writeLocs readLocs
  where
    readScheme = vf ^. vfileLayeredReadScheme
    mbEmbeddedVal = vf ^? vfileEmbeddedValue

    vpath = T.unpack $ toTextRepr $ LTP $ vf ^. vfileOriginalPath

    readers = vf ^. vfileSerials . serialReaders . serialReadersFromStream
    writers = vf ^. vfileSerials . serialWriters . serialWritersToAtomic

    findFunctions :: TypeRep -> HM.HashMap (TypeRep,Maybe FileExt) v -> m' [(v, SomeLocWithVars m)]
    findFunctions typeRep hm | HM.null hm = return []
                             | otherwise  = mapM findFunction layers
      where
        findFunction (SomeGLoc loc) = case HM.lookup (typeRep, Just $ T.pack $ getLocType loc) hm of
          Just f -> return (f, SomeGLoc loc)
          -- TODO: add VirtualFile path to error
          Nothing -> throwM $ TaskConstructionError $
            show loc ++ " is mapped to " ++ vpath ++ " which doesn't support filetype '" ++ getLocType loc ++
            "'. Accepted filetypes here are: " ++
            mconcat (intersperse "," [T.unpack ext | (_,Just ext) <- HM.keys hm]) ++ "."

resolveDataAccess (MbPhysicalFileNode locs _) =
  return $ MbDataAccessNode locs $ First Nothing
