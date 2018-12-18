{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -Wall #-}

module Data.Locations.Mappings
  ( LocationMappings, LocationMappings_(..)
  , HasDefaultMappingRule(..)
  , LocShortcut(..)
  , allLocsInMappings
  , mappingsFromLocTree
  , mappingRootOnly
  , insertMappings
  , propagateMappings
  , applyMappings
  ) where

import           Control.Lens
import           Data.Aeson
import qualified Data.HashMap.Strict                as HM
import           Data.List
import           Data.Locations.Loc
import           Data.Locations.LocationTree
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
type LocationMappings = LocationMappings_ LocShortcut

instance Monoid (LocationMappings_ n) where
  mempty = LocationMappings_ mempty

instance Semigroup (LocationMappings_ n) where
  (LocationMappings_ m) <> (LocationMappings_ m') = LocationMappings_ $
    HM.unionWith (++) m m'

instance (ToJSON n) => ToJSON (LocationMappings_ n) where
  toJSON (LocationMappings_ m) = Object $ HM.fromList $
    map (\(k, v) -> (toTextRepr k, layersToJSON v)) $ HM.toList m
    where
      layersToJSON []     = Null
      layersToJSON [l]    = toJSON l
      layersToJSON layers = toJSON layers

instance FromJSON LocationMappings where
  parseJSON (Object m) = LocationMappings_ . HM.fromList <$>
    mapM (\(k, v) -> (,) <$> fromTextRepr k <*> parseJSONLayers v) (HM.toList m)
    where
      parseJSONLayers Null        = pure []
      parseJSONLayers j@(Array{}) = parseJSON j
      parseJSONLayers j           = (:[]) <$> parseJSON j
  parseJSON _ = mempty

-- | Lists all the physical paths that have been associated to some virtual
-- location
allLocsInMappings :: LocationMappings -> [LocWithVars]
allLocsInMappings (LocationMappings_ m) =
  [ loc
  | (_,layers) <- HM.toList m, FullySpecifiedLoc loc <- layers ]


-- * How to get pre-filled defaut mappings from an existing LocationTree

-- | Means that we can possibly derive a default @LocShortcut@ from @a@
class HasDefaultMappingRule a where
  getDefaultLocShortcut :: a -> Maybe LocShortcut
    -- ^ Nothing means that the @a@ should not be mapped by default

-- | Pre-fills the mappings from the context of a 'LocationTree', with extra
-- metadata saying whether each node should be explicitely mapped or unmapped.
mappingsFromLocTree :: (HasDefaultMappingRule a) => LocationTree a -> LocationMappings
mappingsFromLocTree (LocationTree node subtree) | HM.null subtree =
  LocationMappings_ $
    HM.singleton (LTP [])
                 (case getDefaultLocShortcut node of
                    Just ls -> [ls]
                    Nothing -> [])
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
               [FullySpecifiedLoc $ locWithVarsFromLoc l]


-- * How to parse mappings to and from JSON

-- | A 'LocWithVars' where some parts might have been eluded. Possibly
-- associated to some data.
data LocShortcut = DeriveWholeLocFromTree FileExt
                   -- ^ Means that this loc path should be inherited from locs
                   -- up the resource tree.
                 | DeriveLocPrefixFromTree (LocFilePath LocString)
                 | FullySpecifiedLoc LocWithVars
  deriving (Show)

      -- The underscore sign here means "reuse inherited", depending on the
      -- position it can mean either file path or extension or both.
instance ToJSON LocShortcut where
  toJSON (DeriveWholeLocFromTree ext) = String $ case ext of
    "" -> "_"
    _  -> "_." <> ext
  toJSON (DeriveLocPrefixFromTree l) = String $ "_" <> toTextRepr l
  toJSON (FullySpecifiedLoc l) = String $ toTextRepr l

instance FromJSON LocShortcut where
  parseJSON (String "_") = pure $ DeriveWholeLocFromTree ""
  parseJSON (String (T.uncons -> Just ('_', s))) = case parseLocStringAndExt $ T.unpack s of
    Left e  -> fail e
    Right r -> pure $ DeriveLocPrefixFromTree r
  parseJSON (String s) = case parseURLLikeLoc $ T.unpack s of
    Left e  -> fail e
    Right r -> pure $ FullySpecifiedLoc r
  parseJSON _ = fail "LocShortcut only readable from a JSON String"


-- * How to apply mappings to a LocationTree to get the physical locations bound
-- to each of its nodes

-- | Returns a new 'LocationTree', updated from the mappings. Paths in the
-- 'LocationMappings_' that don't correspond to anything in the 'LocationTree'
-- will just be ignored
insertMappings
  :: LocationMappings
  -> LocationTree a
  -> LocationTree (a, Maybe [LocShortcut])
insertMappings (LocationMappings_ m) tree = foldl' go initTree $ HM.toList m
  where
    initTree = fmap (,Nothing) tree
      -- By defaut, each node is set to "no mapping defined"...
    go t (path, layers) = t &
      inLocTree path . _Just . locTreeNodeTag . _2 .~ Just layers
      -- ...then we update the tree for each mapping present in the
      -- LocationMappings

-- | For each location in the tree, gives it a final list of physical location,
-- as /layers/ (which can be empty)
propagateMappings :: ([LocWithVars] -> a -> Bool -> b)
                  -> LocationTree (a, Maybe [LocShortcut])
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
            sublayers = fmap (</> T.unpack (_ltpiName fname)) theseLayers

-- | Given a list of loc layers inherited from further up the tree, fills in the
-- blanks in the loc shortcuts given for once node of the tree in order to get
-- the final loc layers mapped to this node.
applyInheritedLayersToShortcuts
  :: [LocWithVars] -- ^ Inherited layers
  -> Maybe [LocShortcut] -- ^ LocShortcuts mapped to the node
  -> [LocWithVars] -- ^ Final layers mapped to this node
applyInheritedLayersToShortcuts inheritedLayers Nothing = inheritedLayers
applyInheritedLayersToShortcuts inheritedLayers (Just shortcuts) =
  concatMap fillShortcut shortcuts
  where
    fillShortcut = \case
      FullySpecifiedLoc l -> [l]
      DeriveLocPrefixFromTree fp -> flip map inheritedLayers $
                                    over locFilePath (<> fp)
      DeriveWholeLocFromTree ext -> flip map inheritedLayers $
                                    over locExt (firstNonEmptyExt $ T.unpack ext)

-- | Transform a tree to one where unmapped nodes have been changed to 'mempty'
-- and mapped nodes have been associated to their physical 'Loc'. A function is
-- applied to ask each node to integrate its final mappings, with a Bool to tell
-- whether whether the mapping for a node was explicit (True) or not (False),
-- ie. if it was explicitely declared in the config file or if it was derived
-- from the mapping of a parent folder. @n'@ is often some file type or metadata
-- that's required in the mapping.
applyMappings :: ([LocWithVars] -> a -> Bool -> b)
                                   -- ^ Add physical locations (if they exist) to a node
              -> LocationMappings  -- ^ Mappings to apply
              -> LocationTree a    -- ^ Original tree
              -> LocationTree b    -- ^ Tree with physical locations
applyMappings f mappings loctree =
  propagateMappings f $
    insertMappings mappings loctree
