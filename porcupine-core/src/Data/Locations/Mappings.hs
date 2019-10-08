{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC "-fno-warn-incomplete-uni-patterns" #-}

module Data.Locations.Mappings
  ( LocationMappings, LocationMappings_(..)
  , HasDefaultMappingRule(..)
  , LocShortcut(..), SerializableLocShortcut
  --, allLocsInMappings
  , mappingsFromLocTree
  , mappingRootOnly
  , insertMappings
  , propagateMappings
  , applyMappings
  ) where

import           Control.Arrow                      ((***))
import           Control.Lens
import           Data.Aeson
import qualified Data.HashMap.Strict                as HM
import           Data.List
import           Data.Locations.Accessors
import           Data.Locations.Loc
import           Data.Locations.LocationTree
import           Data.Locations.LogAndErrors
import           Data.Locations.SerializationMethod (FileExt)
import           Data.Maybe
import           Data.Representable
import qualified Data.Text                          as T


-- * The 'LocationMappings' type

newtype LocationMappings_ n = LocationMappings_
  (HM.HashMap LocationTreePath [n])
  deriving (Functor, Show)

-- | Describes how physical locations are mapped to an application's
-- LocationTree. This is the type that is written to the pipeline yaml config
-- file under the "locations" section.
type LocationMappings = LocationMappings_ SerializableLocShortcut

instance Monoid (LocationMappings_ n) where
  mempty = LocationMappings_ mempty

instance Semigroup (LocationMappings_ n) where
  (LocationMappings_ m) <> (LocationMappings_ m') = LocationMappings_ $
    HM.unionWith (++) m m'

instance (ToJSON n) => ToJSON (LocationMappings_ n) where
  toJSON (LocationMappings_ m) = Object $ HM.fromList $
    map (toTextRepr *** layersToJSON) $ HM.toList m
    where
      layersToJSON []     = Null
      layersToJSON [l]    = toJSON l
      layersToJSON layers = toJSON layers

instance FromJSON LocationMappings where
  parseJSON (Object m) = LocationMappings_ . HM.fromList <$>
    mapM (\(k, v) -> (,) <$> fromTextRepr k <*> parseJSONLayers v) (HM.toList m)
    where
      parseJSONLayers Null      = pure []
      parseJSONLayers j@Array{} = parseJSON j
      parseJSONLayers j         = (:[]) <$> parseJSON j
  parseJSON _ = mempty

-- -- | Lists all the physical paths that have been associated to some virtual
-- -- location
-- allLocsInMappings :: LocationMappings -> [LocWithVars]
-- allLocsInMappings (LocationMappings_ m) =
--   [ loc
--   | (_,layers) <- HM.toList m, FullySpecifiedLoc loc <- layers ]


-- * How to get pre-filled defaut mappings from an existing LocationTree

-- | Means that we can possibly derive a default @LocShortcut@ from @a@
class HasDefaultMappingRule a where
  getDefaultLocShortcut :: a -> Maybe (LocShortcut x)
    -- ^ Nothing means that the @a@ should not be mapped by default

-- | Pre-fills the mappings from the context of a 'LocationTree', with extra
-- metadata saying whether each node should be explicitely mapped or unmapped.
mappingsFromLocTree :: (HasDefaultMappingRule a) => LocationTree a -> LocationMappings
mappingsFromLocTree (LocationTree node subtree) | HM.null subtree =
  LocationMappings_ $
    HM.singleton (LTP [])
                 (case getDefaultLocShortcut node of
                    Just shortcuts -> [shortcuts]
                    Nothing        -> [])
mappingsFromLocTree (LocationTree _ sub) =
  LocationMappings_ (mconcat $ map f $ HM.toList sub)
  where
    f (ltpi, t) =
      HM.fromList $ map appendPath $ HM.toList m
      where
        appendPath (LTP path, maps) = (LTP $ ltpi : path, maps)
        LocationMappings_ m = mappingsFromLocTree t

-- | Creates a 'LocationMappings_' where the whole LocationTree is mapped to a
-- single folder
mappingRootOnly :: Loc -> LocationMappings
mappingRootOnly l = LocationMappings_ $
  HM.singleton (LTP [])
               [FullySpecifiedLoc $ toJSON $ locWithVarsFromLoc l]


-- * How to parse mappings to and from JSON

-- | A location with variables where some parts may have been eluded
data LocShortcut a
  = DeriveWholeLocFromTree FileExt
    -- ^ Means that this loc path and name should be inherited from locs up the
    -- virtual tree.
  | DeriveLocPrefixFromTree (PathWithExtension StringWithVars)
    -- ^ Means that this loc path should be inherited from locs up the resource
    -- tree. Its name should be a concatenation of the corresponding name in the
    -- tree and the PathWithExtension provided
  | FullySpecifiedLoc a
    -- ^ Means that this shortcut is a full location
  deriving (Show)

-- | A 'LocShorcut' where fully specified locs are aeson Values ready to be
-- parsed by some LocationAccessor. It is parsed from the mappings in the
-- configuration file.
type SerializableLocShortcut = LocShortcut Value

-- | A 'LocShortcut' where fully specified locs have been parsed, and resolved
-- to be tied to some specific LocationAccessor. It isn't serializable in JSON,
-- hence the separation between this and 'SerializableLocShortcut'.
type ResolvedLocShortcut m = LocShortcut (SomeLocWithVars m)

      -- The underscore sign here means "reuse inherited", depending on the
      -- position it can mean either file path or extension or both.
instance ToJSON SerializableLocShortcut where
  toJSON (DeriveWholeLocFromTree ext) = String $ case ext of
    "" -> "_"
    _  -> "_." <> ext
  toJSON (DeriveLocPrefixFromTree l) = String $ "_" <> toTextRepr l
  toJSON (FullySpecifiedLoc v) = v

instance FromJSON SerializableLocShortcut where
  parseJSON (String "_") = pure $ DeriveWholeLocFromTree ""
  parseJSON (String (T.uncons -> Just ('_', s))) = case parseLocStringAndExt $ T.unpack s of
    Left e  -> fail e
    Right r -> pure $ DeriveLocPrefixFromTree r
  parseJSON v = pure $ FullySpecifiedLoc v

-- * How to apply mappings to a LocationTree to get the physical locations bound
-- to each of its nodes

-- | Returns a new 'LocationTree', updated from the mappings. Paths in the
-- 'LocationMappings_' that don't correspond to anything in the 'LocationTree'
-- will just be ignored
insertMappings
  :: LocationMappings
  -> LocationTree a
  -> LocationTree (a, Maybe [SerializableLocShortcut])
insertMappings (LocationMappings_ m) tree = foldl' go initTree $ HM.toList m
  where
    initTree = fmap (,Nothing) tree
      -- By defaut, each node is set to "no mapping defined"...
    go t (path, layers) = t &
      inLocTree path . _Just . locTreeNodeTag . _2 .~ Just layers
      -- ...then we update the tree for each mapping present in the
      -- LocationMappings

-- | For each location in the tree, gives it a final list of physical locations,
-- as /layers/ (which can be empty)
propagateMappings :: forall m a b.
                     ([SomeLocWithVars m] -> a -> Bool -> b)
                  -> LocationTree (a, Maybe [ResolvedLocShortcut m])
                  -> LocationTree b
propagateMappings f tree = propagateMappings' [] tree
  where
    -- if a folder is explicitly set to null (ie if no layer exist for this
    -- folder), then we recursively unmap everything is contains, ignoring every
    -- submapping that might exist:
    propagateMappings' _ t@(LocationTree (_, Just []) _) = fmap unmap t
      where unmap (n, _) = f [] n True
    -- if a folder is mapped, we propagate the mapping downwards:
    propagateMappings' inheritedLayers (LocationTree (thisNode, mbTheseMappings) thisSub) =
      LocationTree thisNode' $ imap recur thisSub
      where
        theseLayers = applyInheritedLayersToShortcuts inheritedLayers mbTheseMappings
        thisNode' = f theseLayers thisNode (isJust mbTheseMappings)
        recur fname subtree = propagateMappings' sublayers subtree
          where
            addSubdir :: SomeLocWithVars m -> SomeLocWithVars m
            addSubdir (SomeGLoc l) = SomeGLoc $ addSubdirToLoc l $ T.unpack (_ltpiName fname)
            sublayers = fmap addSubdir theseLayers

-- | Given a list of loc layers inherited from further up the tree, fills in the
-- blanks in the loc shortcuts given for once node of the tree in order to get
-- the final loc layers mapped to this node.
applyInheritedLayersToShortcuts
  :: forall m.
     [SomeLocWithVars m] -- ^ Inherited layers
  -> Maybe [ResolvedLocShortcut m] -- ^ LocShortcuts mapped to the node
  -> [SomeLocWithVars m] -- ^ Final layers mapped to this node
applyInheritedLayersToShortcuts inheritedLayers Nothing = inheritedLayers
applyInheritedLayersToShortcuts inheritedLayers (Just shortcuts) =
  concatMap fillShortcut shortcuts
  where
    fillShortcut = \case
      FullySpecifiedLoc l -> [l]
      DeriveLocPrefixFromTree fp ->
        flip map inheritedLayers $ \(SomeGLoc l) ->
                                     SomeGLoc @m $ useLocAsPrefix l fp
      DeriveWholeLocFromTree ext ->
        flip map inheritedLayers $ \(SomeGLoc l) ->
                                     SomeGLoc @m $ overrideLocType l (T.unpack ext)

-- | In a context where we have LocationAccessors available, we parse the
-- locations in a 'SerializableLocShortcut' and obtain a 'ResolvedLocShortcut'
resolveLocShortcut :: (LogThrow m) => SerializableLocShortcut -> LocResolutionM m (ResolvedLocShortcut m)
resolveLocShortcut (DeriveWholeLocFromTree ext) = return $ DeriveWholeLocFromTree ext
resolveLocShortcut (DeriveLocPrefixFromTree path) = return $ DeriveLocPrefixFromTree path
resolveLocShortcut (FullySpecifiedLoc value) =
  withParsedLocsWithVars [value] $ \[resolvedLoc] ->
    return $ FullySpecifiedLoc $ SomeGLoc resolvedLoc

-- | Transform a tree to one where unmapped nodes have been changed to 'mempty'
-- and mapped nodes have been associated to their physical location. A function
-- is applied to ask each node to integrate its final mappings, with a Bool to
-- tell whether whether the mapping for a node was explicit (True) or not
-- (False), ie. if it was explicitely declared in the config file or if it was
-- derived from the mapping of a parent folder. @n'@ is often some file type or
-- metadata that's required in the mapping.
--
-- TODO: Maybe change the callback type to
-- @DerivedOrExplicit [SomeLocWithVars m] -> a -> b@ with
-- @data DerivedOrExplicit a = Derived a | Explicit a@
applyMappings :: (LogThrow m)
              => ([SomeLocWithVars m] -> a -> Bool -> b)
                                   -- ^ Add physical locations (if they exist)
                                   -- to a node
              -> LocationMappings  -- ^ Mappings to apply
              -> LocationTree a    -- ^ Original tree
              -> LocResolutionM m (LocationTree b) -- ^ Tree with physical locations
applyMappings f mappings loctree = do
  let treeWithShortcuts = insertMappings mappings loctree
      resolve (node, Nothing) = return (node, Nothing)
      resolve (node, Just shortcuts) =
        (\rs -> (node, Just rs)) <$> mapM resolveLocShortcut shortcuts
  treeWithResolvedShortcuts <- traverse resolve treeWithShortcuts
  return $ propagateMappings f treeWithResolvedShortcuts
