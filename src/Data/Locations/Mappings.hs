{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

module Data.Locations.Mappings
  ( LocationMappings
  , HasDefaultMappingRule(..)
  , LocShortcut(..)
  , Mapping
  , allLocsInMappings
  , mappingsFromLocTree
  , mappingRootOnly
  , insertMappings
  , propagateMappings
  , applyMappings
  ) where

import           Control.Lens
import           Data.Aeson
import qualified Data.HashMap.Strict         as HM
import           Data.List
import           Data.Locations.Loc
import           Data.Locations.LocationTree
import           Data.Locations.SerializationMethod (FileExt)
import           Data.Maybe
import           Data.Representable
import qualified Data.Text                   as T
import           GHC.Generics


-- * The 'LocationMappings' type

newtype LocationMappings_ n = LocationMappings_
  (HM.HashMap LocationTreePath (Mapping n))
  deriving (Functor, Show)

-- | Describes how physical locations are mapped to an application's
-- LocationTree. This is the type that is written to the pipeline yaml config
-- file under the "locations" section.
type LocationMappings = LocationMappings_ LocShortcut

instance Monoid (LocationMappings_ n) where
  mempty = LocationMappings_ mempty

instance Semigroup (LocationMappings_ n) where
  (LocationMappings_ m) <> (LocationMappings_ m') = LocationMappings_ $
    HM.unionWith f m m'
    where
      f Unmapped a                 = a
      f a Unmapped                 = a
      f (MappedTo l) (MappedTo l') = MappedTo (l ++ l')

instance (ToJSON n) => ToJSON (LocationMappings_ n) where
  toJSON (LocationMappings_ m) = Object $ HM.fromList $
    map (\(k, v) -> (toTextRepr k, toJSON v)) $ HM.toList m

instance FromJSON LocationMappings where
  parseJSON (Object m) = LocationMappings_ . HM.fromList <$>
    mapM (\(k, v) -> (,) <$> fromTextRepr k <*> parseJSON v) (HM.toList m)
  parseJSON _ = mempty

-- | Lists all the physical paths that have been associated to some virtual
-- location
allLocsInMappings :: LocationMappings -> [LocWithVars]
allLocsInMappings (LocationMappings_ m) =
  concat . mapMaybe (f . snd) $ HM.toList m
  where
    f Unmapped     = Nothing
    f (MappedTo a) = Just $ mapMaybe g a
    g (FullySpecifiedLoc l) = Just l
    g _ = Nothing


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
                    Just ls -> MappedTo [ls]
                    Nothing -> Unmapped)
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
  HM.fromList [( LTP []
               , MappedTo [FullySpecifiedLoc (locWithVarsFromLoc l)] )]


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
    Left s' -> fail s'
    Right r -> pure $ DeriveLocPrefixFromTree r
  parseJSON (String s) = case parseURL $ T.unpack s of
    Left s' -> fail s'
    Right r -> pure $ FullySpecifiedLoc r
  parseJSON _ = fail "LocShortcut only readable from a JSON String"

-- | If a ressource is mapped, it's mapped to a series of _layers_, each
-- subsequent layer having the capacity to override the previous ones.
data Mapping n = Unmapped | MappedTo [n]
  deriving (Eq, Show, Functor, Generic)

instance (ToJSON n) => ToJSON (Mapping n) where
  toJSON = \case
    Unmapped      -> Null
    MappedTo [ln] -> toJSON ln
    MappedTo p    -> toJSON $ map toJSON p

instance (FromJSON n) => FromJSON (Mapping n) where
  parseJSON Null = pure Unmapped
  parseJSON j@(Array{}) = MappedTo <$> parseJSON j
  parseJSON j = MappedTo . (:[]) <$> parseJSON j


-- * How to apply mappings to a LocationTree to get the physical locations bound
-- to each of its nodes

-- | Returns a new 'LocationTree', updated from the mappings. Paths in the
-- 'LocationMappings_' that don't correspond to anything in the 'LocationTree'
-- will just be ignored
insertMappings
  :: LocationMappings_ n' -> LocationTree n -> LocationTree (n, Mapping n')
insertMappings (LocationMappings_ m) tree = foldl' go initTree $ HM.toList m
  where
    initTree = fmap (,MappedTo []) tree  -- No mapping declared means an empty
                                         -- list of layers
    go t (path, lw) = t & inLocTree path . _Just . locTreeNodeTag . _2 .~ lw

-- | For each location in the tree, gives it a final list of physical location,
-- as /layers/ (which can be empty)
propagateMappings :: ([LocWithVars] -> n -> Bool -> n'')
                  -> LocationTree (n, Mapping LocShortcut)
                  -> LocationTree n''
propagateMappings f tree = propagateMappings' [] tree
  where
    -- if a folder is set to null, we recursively unmap everything is contains,
    -- ignoring every submapping that might exist:
    propagateMappings' _ t@(LocationTree (_, Unmapped) _) = fmap unmap t
      where unmap (n, _) = f [] n True
    -- if a folder is mapped, we propagate the mapping downwards:
    propagateMappings' inheritedLayers (LocationTree (thisNode, MappedTo theseMappings) thisSub) =
      LocationTree thisNode' $ imap recur thisSub
      where
        theseLayers = applyMappingsToLayers inheritedLayers theseMappings
        thisNode' = f theseLayers thisNode (not $ null theseMappings)
        recur fname subtree = propagateMappings' sublayers subtree
          where
            sublayers = fmap (</> T.unpack (_ltpiName fname)) theseLayers 

-- | Given a list of loc layers inherited from further up the tree, fills in the
-- blanks in the loc shortcuts given for once node of the tree in order to get
-- the final loc layers mapped to this node.
applyMappingsToLayers :: [LocWithVars] -- ^ Inherited layers
                      -> [LocShortcut] -- ^ LocShortcuts mapped to the node
                      -> [LocWithVars] -- ^ Final layers mapped to this node
applyMappingsToLayers inheritedLayers = concatMap fillShortcut
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
applyMappings :: ([LocWithVars] -> n -> Bool -> n'')
                                   -- ^ Add physical locations (if they exist) to a node
              -> LocationMappings  -- ^ Mappings to apply
              -> LocationTree n       -- ^ Original tree
              -> LocationTree n''  -- ^ Tree with physical locations
applyMappings f mappings loctree =
  propagateMappings f $
    insertMappings mappings loctree
