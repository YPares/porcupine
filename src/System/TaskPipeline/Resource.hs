{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -Wall #-}

module System.TaskPipeline.Resource
 ( PipelineResource
 , UnboundPipelineResource
 , BoundPipelineResource
 , pattern PRscOptions
 , pattern PRscVirtualFile
 , pattern PRscSerialMethod
 , pattern PRscJustOneLayer
 , pattern PRscNothing
 , RecOfOptions(..)
 , DocRecOfOptions
 , UnboundResourceTree
 , BoundResourceTree
 , ResourceTreeAndMappings(..)
 , pRscOptions
 , pRscVirtualFile
 , applyOneRscMapping
 , rscTreeBasedCLIOverriding
 , applyMappingsToResourceTree
 , applyMappingsToResourceTree'

 , rscTreeToMappings, rscTreeToOptionTree
 ) where

import           Control.Lens
import qualified Data.Aeson                         as A
import           Data.Default
import           Data.DocRecord
import           Data.DocRecord.OptParse
import qualified Data.HashMap.Strict                as HM
import           Data.List
import           Data.Locations
import           Data.Maybe
import           Data.Monoid
import           Data.Representable
import qualified Data.Text                          as T
import           Data.Typeable
import           Options.Applicative
import           System.TaskPipeline.CLI.Overriding


-- | Contains any set of options that should be exposed via the CLI
data RecOfOptions field = forall rs.
  (Typeable rs, RecordUsableWithCLI rs)
  => RecOfOptions (Rec field rs)

type DocRecOfOptions = RecOfOptions DocField

-- | A more general version of 'PipelineResource' that doesn't constrain the
-- records of options to be DocRecords (extra layers, eg. to add tags to every
-- field, might have been added for instance). @f@ should either be
-- @WithDefaultUsage@ or @LocLayers@, depending on whether this PipelineResource
-- is yet to be bound to definitive values or paths, or whether is has been
-- bound.
newtype PipelineResource_ fld f = PRsc (Last (Either (RecOfOptions fld) (f SerialMethod)))
  deriving (Semigroup, Monoid)

-- | The type of resources that tasks running in the pipeline will try to
-- access: either virtual files (see 'PRscVirtualFile') defined by a
-- serialization method (JSON, CSV...) or options (that would have been obtained
-- from the command-line).
type PipelineResource = PipelineResource_ DocField

-- | A resource required by tasks, not yet bound to any layer but possibly
-- specifying a default mapping to use if the user doesn't give one.
type UnboundPipelineResource = PipelineResource WithDefaultUsage

-- | A resource mapped in the yaml config file to one or several layers
type BoundPipelineResource = PipelineResource LocLayers

-- | The type of tree than contains the resources required by a task or a full
-- task pipeline
type UnboundResourceTree = LocationTree UnboundPipelineResource

-- | A resource tree that has its locations attached to physical locations
type BoundResourceTree = LocationTree BoundPipelineResource

pRsc
  :: Functor f1
  => (Maybe (Either (RecOfOptions fld) (f2 SerialMethod))
  -> f1 (Maybe (Either (RecOfOptions fld') (f3 SerialMethod))))
  -> PipelineResource_ fld f2
  -> f1 (PipelineResource_ fld' f3)
pRsc = lens (\(PRsc (Last r)) -> r) (\(PRsc _) r -> PRsc (Last r))

-- | Used for unbound and bound 'PipelineResource's. Means that the location
-- will correspond to a set of options, not a virtual file waiting to be bound.
pattern PRscOptions :: RecOfOptions fld -> PipelineResource_ fld f
pattern PRscOptions opts = PRsc (Last (Just (Left opts)))

-- | A Traversal to the RecOfOptions possibly contained in the
-- 'PipelineResource'.
pRscOptions :: Traversal (PipelineResource_ fld f) (PipelineResource_ fld' f) (RecOfOptions fld) (RecOfOptions fld')
pRscOptions = pRsc . _Just . _Left

-- | Used for unbound and bound 'PipelineResource's.
pattern PRscVirtualFile :: f SerialMethod -> PipelineResource_ fld f
pattern PRscVirtualFile x = PRsc (Last (Just (Right x)))

-- | Subcase of 'PRscVirtualFile' used only for unbound resources. Means that
-- the expected virtual file is by default expected to have some
-- SerialMethod. When used as as constructor, this resource default usage is
-- always True.
pattern PRscSerialMethod :: SerialMethod -> PipelineResource_ fld WithDefaultUsage
pattern PRscSerialMethod x <- PRscVirtualFile (WithDefaultUsage _ x) where
  PRscSerialMethod x = PRscVirtualFile $ WithDefaultUsage True x

-- | Used only for bound resources. Means that the virtual file is bound to one
-- and only one physical file.
pattern PRscJustOneLayer :: Loc -> SerialMethod -> PipelineResource_ fld LocLayers
pattern PRscJustOneLayer l s = PRscVirtualFile (LocLayers (l,s) [])

-- | A Traversal to a virtual file metadata possibly contained in the
-- 'PipelineResource'.
pRscVirtualFile :: Traversal' (PipelineResource_ fld f) (f SerialMethod)
pRscVirtualFile = pRsc . _Just . _Right

-- | On unbound resources, 'PRscNothing' is used for intermediate folders (no
-- options nor files at this point). On bound resources, it's more general, it
-- means that the resource shouldn't be used (if it's a file, it shouldn't be
-- loaded, see 'layeredAccessTask'. If it's a rec of options, then it means the
-- user hasn't specified it, and therefore a set of default options should be
-- used by the task, see 'getOptionsTask').
pattern PRscNothing :: PipelineResource_ fld f
pattern PRscNothing = PRsc (Last Nothing)

instance Show UnboundPipelineResource where
  show PRscNothing            = ""
  show PRscOptions{}          = "<record of options>"
  show (PRscSerialMethod ser) = show ser
  show _                      = "???"

instance Show BoundPipelineResource where
  show PRscNothing   = "null"
  show PRscOptions{} = "<record of options>"
  show a = case layers of
    [] -> "???"
    _  -> t
    where
      t = T.unpack $ mconcat $ intersperse " << " layers
      layers = a ^.. pRscVirtualFile . locLayers . to repr
      repr (path,LocDefault) = toTextRepr path
      repr (path,ser) = toTextRepr $
        addExtToLocIfMissing path ser


-- | Get pre-filled mappings from an 'UnboundResourceTree'.
rscTreeToMappings :: UnboundResourceTree -> Maybe (LocationMappings SerialMethod)
rscTreeToMappings tree = mappingsFromLocTree <$> over filteredLocsInTree rm tree
  where
    rm (PRscVirtualFile m) = Just m
    rm PRscNothing         = Just $ WithDefaultUsage True def
                             -- Intermediary levels (folders) are kepy, and just
                             -- mapped mapped to LocDefault.
    rm _                   = Nothing

-- | Filters an 'UnboundResourceTree' so it contains only the records of default
-- options and the intermediate folders containing them.
rscTreeToOptionTree :: UnboundResourceTree -> Maybe (LocationTree (Maybe DocRecOfOptions))
rscTreeToOptionTree = over filteredLocsInTree rm
  where
    rm (PRscOptions o) = Just $ Just o
    rm PRscNothing     = Just Nothing
                         -- Intermediary levels (folders) without options must
                         -- be kept
    rm _               = Nothing

optionTreeToJSONFields
  :: T.Text -> LocationTree (Maybe DocRecOfOptions) -> [(T.Text, A.Value)]
optionTreeToJSONFields thisPath (LocationTree mbOpts sub) =
  [(thisPath, A.Object $ opts' <> sub')]
  where
    opts' = case mbOpts of
      Just (RecOfOptions (A.toJSON -> A.Object o)) -> o
      _                                            -> mempty
    sub' = HM.fromList $
      concat $ map (\(k,v) -> optionTreeToJSONFields (_ltpiName k) v) $ HM.toList sub

-- | An 'UnboundResourceTree' associated with full mappings (or just a top root loc)
-- that should be applied to it
data ResourceTreeAndMappings =
  ResourceTreeAndMappings UnboundResourceTree (Either Loc (LocationMappings SerialMethod))

mappingsYamlSection :: T.Text
mappingsYamlSection = "locations"

optionsYamlSection :: T.Text
optionsYamlSection = "options"

instance A.ToJSON ResourceTreeAndMappings where
  toJSON (ResourceTreeAndMappings tree mappings) = A.Object $ HM.fromList $
    -- In the JSON representation of the UnboundResourceTree, we separate the pure file
    -- locations (which we show in their mapping form, ie. full paths) from the
    -- DocRecs of options.
    (case rscTreeToMappings tree of
       Just m -> [(mappingsYamlSection, A.toJSON $ case mappings of
                      Right m'     -> m'
                      Left rootLoc -> mappingRootOnly rootLoc <> m)]
       Nothing -> [])
    ++
    (case rscTreeToOptionTree tree of
       Just t  -> optionTreeToJSONFields optionsYamlSection t
       Nothing -> [])

-- | Apply the mappings read in configuration file to the combined
-- 'LocationTree' requested by the pipeline, by adding the mapped Locs to the
-- corresponding nodes of the tree
applyOneRscMapping
  :: Functor f
  => f (Maybe SerialMethod)
  -> UnboundPipelineResource -> PipelineResource f
applyOneRscMapping layers (PRscSerialMethod serMeth) =
  PRscVirtualFile $ fmap (fromMaybe serMeth) layers
applyOneRscMapping layers PRscNothing =
  PRscVirtualFile $ fmap (fromMaybe def) layers
applyOneRscMapping _ (PRscOptions o) = PRscOptions o
  -- We never apply any mapping to options, we just use them as they are
applyOneRscMapping _ _ =
  error "applyOneMapping: should not happen, pattern matching coverage is normally full"

-- | Returns the final BoundResourceTree, with its location mappings integrated
applyMappingsToResourceTree :: ResourceTreeAndMappings -> BoundResourceTree
applyMappingsToResourceTree (ResourceTreeAndMappings tree mappings) =
  applyMappingsToResourceTree' tree mappings

-- | Shortcut for 'applyMappingstoResourceTree (ResourceTreeAndMappings t m)'
applyMappingsToResourceTree'
  :: UnboundResourceTree
  -> Either Loc (LocationMappings SerialMethod)
  -> BoundResourceTree
applyMappingsToResourceTree' tree mappings =
  applyMappings applyOneRscMapping m' tree
  where
    m' = case mappings of
      Right m      -> m
      Left rootLoc -> mappingRootOnly rootLoc

-- | Works on a tree of PipelineResources and permits to override the whole of
-- the tree (options and file mappings) through the Yaml configuration file and
-- the CLI arguments
rscTreeBasedCLIOverriding
  :: ResourceTreeAndMappings
  -> CLIOverriding ResourceTreeAndMappings (LocationTree (PipelineResource_ SourcedDocField WithDefaultUsage))
rscTreeBasedCLIOverriding (ResourceTreeAndMappings defTree _) = CLIOverriding{..}
  where
    overridesParser =
      traverseOf (traversed.pRscOptions) parseOptions defTree

    parseOptions :: RecOfOptions DocField -> Parser (RecOfOptions SourcedDocField)
    parseOptions (RecOfOptions r) = RecOfOptions <$>
      -- parseRecFromCLI will update the source tag to 'Src.CLI' for the fields
      -- updated by the user on the CLI
      parseRecFromCLI (tagWithDefaultSource r)

    nullOverrides = allOf (traversed.pRscOptions) nullRec

    nullRec (RecOfOptions RNil) = True
    nullRec _                   = False

    overrideCfgFromYamlFile
      :: A.Value
      -> LocationTree (PipelineResource_ SourcedDocField WithDefaultUsage)
      -> ([String], Either String ResourceTreeAndMappings)
    overrideCfgFromYamlFile aesonCfg optsTree =
      ([], do
          ResourceTreeAndMappings
            <$> traverseOf allSubLocTrees integrateAesonCfg optsTree
            <*> (Right <$> getMappings))
      where
        getMappings = case aesonCfg of
          A.Object (HM.lookup mappingsYamlSection -> Just m) -> parseJSONEither m
          _ -> Left $ "Location mappings not found in the Yaml config"
        integrateAesonCfg (path@(LTP path'), node) =
          traverseOf pRscOptions integrateOne $ _locTreeNodeTag node
          where
            findInAesonVal [] v = return v
            findInAesonVal (p:ps) (A.Object (HM.lookup p -> Just v)) = findInAesonVal ps v
            findInAesonVal _ _ = Left $ "rscTreeBasedCLIOverriding: " ++
              (T.unpack $ toTextRepr path) ++ " doesn't match any path in the Yaml config"
            integrateOne (RecOfOptions recFromCLI) = do
              v <- findInAesonVal (optionsYamlSection : map _ltpiName path') aesonCfg
              recFromYaml <- tagWithYamlSource <$> parseJSONEither v
              pure $ RecOfOptions $ rmTags $ rzipWith chooseHighestPriority recFromYaml recFromCLI
