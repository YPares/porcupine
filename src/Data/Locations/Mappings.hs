{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -Wall #-}

module Data.Locations.Mappings
  ( LocationMappings
  , WithDefaultUsage(..)
  , HasDefautUsage(..)
  , LocLayers(..)
  , Mapping
  , allLocsInMappings
  , mappingsFromLocTree
  , locLayers
  , mappingRootOnly
  , getLocMappedToRoot
  , insertMappings
  , propagateMappings
  , applyMappings
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.Binary
import           Data.Binary.Orphans         ()
import           Data.Default
import           Data.Either
import           Data.Function               (on)
import qualified Data.HashMap.Strict         as HM
import           Data.List
import           Data.Locations.Loc
import           Data.Locations.LocationTree
import           Data.Maybe
import           Data.Representable
import qualified Data.Text                   as T
import           GHC.Generics
import           System.FilePath             (splitExtension)


-- | Describes how physical locations are mapped to an application's
-- LocationTree. This is the type that is written to the pipeline yaml config
-- file under the "locations" section.
newtype LocationMappings n = LocationMappings
  (HM.HashMap LocationTreePath (Mapping n))
  deriving (Functor, Binary, Show)

instance Monoid (LocationMappings n) where
  mempty = LocationMappings mempty

instance Semigroup (LocationMappings n) where
  (LocationMappings m) <> (LocationMappings m') = LocationMappings $
    HM.unionWith f m m'
    where
      f Unmapped a                 = a
      f a Unmapped                 = a
      f (MappedTo l) (MappedTo l') = MappedTo (l ++ l')

instance (Representable n) => ToJSON (LocationMappings n) where
  toJSON (LocationMappings m) = Object $ HM.fromList $
    map (\(k, v) -> (toTextRepr k, toJSON v)) $ HM.toList m

instance FromJSON (LocationMappings SerialMethod) where
  parseJSON (Object m) = LocationMappings . HM.fromList <$>
    mapM (\(k, v) -> (,) <$> fromTextRepr k <*> parseJSON v) (HM.toList m)
  parseJSON _ = mempty

-- | Lists all the physical paths that have been associated to some virtual
-- location
allLocsInMappings :: LocationMappings n -> [Loc]
allLocsInMappings (LocationMappings m) =
  concat . mapMaybe (f . snd) $ HM.toList m
  where
    f Unmapped     = Nothing
    f (MappedTo a) = Just $ mapMaybe fst a

-- Temporary
class HasDefautUsage a b | a -> b where
  isUsedByDefault :: a -> Bool
  hasDefaultUsageInner :: a -> b

-- | Pre-fills the mappings from the context of a 'LocationTree', with extra
-- metadata saying whether each node should be explicitely mapped or unmapped.
mappingsFromLocTree :: (HasDefautUsage a b) => LocationTree a -> LocationMappings b
mappingsFromLocTree (LocationTree node (HM.toList -> [])) =
  LocationMappings $
    HM.fromList [(LTP []
                 , if isUsedByDefault node
                   then MappedTo [(Nothing, Just $ hasDefaultUsageInner node)]
                   else Unmapped)]
mappingsFromLocTree (LocationTree _ sub) =
  LocationMappings (mconcat $ map f $ HM.toList sub)
  where
    f (ltpi, t) =
      HM.fromList $ map appendPath $ HM.toList m
      where
        appendPath (LTP path, maps) = (LTP $ ltpi : path, maps)
        LocationMappings m = mappingsFromLocTree t

-- | If a ressource is mapped, it's mapped to a series of _layers_, each
-- subsequent layer having the capacity to override the previous ones. When a
-- layer's Loc is 'Nothing', then it means that this layer's path should be
-- inherited from layers up the tree.
data Mapping n = Unmapped | MappedTo [(Maybe Loc, Maybe n)]
  deriving (Eq, Show, Functor, Generic)

instance (Binary n) => Binary (Mapping n)

instance (Representable n) => ToJSON (Mapping n) where
  toJSON locs = case locs of
    Unmapped      -> Null
    MappedTo [ln] -> toJ ln
    MappedTo p    -> toJSON $ map toJ p
    where
      toJ (Nothing, Nothing) = String "_"
      toJ (Nothing, Just n)  = String $ case toTextRepr n of
        ""  -> "_"
        ext -> "_." <> ext
      toJ (Just l, Nothing)  = String $ toTextRepr l
      toJ (Just l, Just n)   = Object $ HM.fromList
        [(toTextRepr l, String $ toTextRepr n)]

instance FromJSON (Mapping SerialMethod) where
  parseJSON Null                           = pure Unmapped
  parseJSON (String (T.toLower -> "none")) = pure Unmapped
  parseJSON j = MappedTo <$> readPathAndFormat j
    where
      -- The underscore sign here means "reuse inherited", depending on the
      -- position it can mean either file path or extension or both.
      readPathAndFormat (String "_") = pure
        [(Nothing, Nothing)]
      readPathAndFormat (String s) =
        case splitExtension $ T.unpack s of
          (path, "") -> pure [(fromTextRepr $ T.pack path, Nothing)]
          (path, ext) ->
            let ext' = case ext of
                  '.' : e -> e
                  e       -> e
                path' = case path of
                  "_" -> Nothing
                  _   -> fromTextRepr $ T.pack path
            in map (path',) <$> case ext' of
                                  "_" -> pure [Nothing]
                                  _   -> parseSerMethList (String $ T.pack ext')
      readPathAndFormat (Object (HM.toList -> [(l, fmts)])) = do
        -- checking that we are matching for a single key based object
        fmts' <- parseSerMethList fmts
        case (fmts', null $ snd $ splitExtension $ T.unpack l) of
          (_:_:_, False) -> fail $ "'" ++ T.unpack l ++
            "' is invalid: a file cannot contain an extension when mapped to several types"
          _ -> return $ map (fromTextRepr l,) fmts'
      readPathAndFormat (Array m)    = concat <$>
        mapM readPathAndFormat (toListOf traversed m)
      readPathAndFormat _            = fail
        "A location mapping must either be a map of one 'path: format' entry,\
        \ be just a 'format' string or be an array of some of these."

-- | Just packs a value with a Bool indicating whether by default, this value
-- should be used or not.
data WithDefaultUsage a = WithDefaultUsage Bool a

instance HasDefautUsage (WithDefaultUsage a) a where
  isUsedByDefault (WithDefaultUsage b _) = b
  hasDefaultUsageInner (WithDefaultUsage _ x) = x


-- | This type is used to indicate that a location in a 'LocationTree' is mapped
-- not to one but to several physical locs, at least one (LocLayers behaves like
-- a never-empty stack). The Locs on the left take precedence and
-- override these on the right. @a@ is often a filetype here.
data LocLayers a = LocLayers
  { _topLocLayer    :: (Loc, a)
  , _otherlocLayers :: [(Loc, a)] }
  deriving (Functor, Foldable, Traversable, Show)

-- | Traverse the layers from top to bottom
locLayers :: Traversal (LocLayers a) (LocLayers b) (Loc, a) (Loc, b)
locLayers f (LocLayers l ls) = LocLayers <$> f l <*> traverse f ls

-- | Creates a 'LocationMappings' where the whole LocationTree is mapped to a
-- single folder
mappingRootOnly :: Loc -> LocationMappings SerialMethod
mappingRootOnly l = LocationMappings $ HM.fromList [(LTP [], MappedTo [(Just l, def)])]

-- | Gets the last layer folder mapped to the root. Returns Nothing if the root
-- has several mappings.
getLocMappedToRoot :: LocationMappings n -> Maybe Loc
getLocMappedToRoot (LocationMappings h) = case HM.lookup (LTP []) h of
  Just (MappedTo (reverse -> (Just l, _):_)) -> Just l
  _                                          -> Nothing

-- | Returns a new 'LocationTree', updated from the mappings. Paths in the
-- 'LocationMappings' that don't correspond to anything in the 'LocationTree'
-- will just be ignored
insertMappings
  :: LocationMappings n' -> LocationTree n -> LocationTree (n, Mapping n')
insertMappings (LocationMappings m) tree = foldl' go initTree $ HM.toList m
  where
    initTree = fmap (,MappedTo []) tree  -- No mapping declared means an empty
                                         -- list of layers
    go t (path, lw) = t & inLocTree path . _Just . locTreeNodeTag . _2 .~ lw

-- | For each location in the tree, gives it a final, physical location or
-- just put Nothing if it isn't mapped
propagateMappings :: (Monoid n'')
                  => (LocLayers (Maybe n') -> n -> n'')
                  -> LocationTree (n, Mapping n')
                  -> LocationTree n''
propagateMappings f tree = propagateMappings' Nothing tree
  where
    propagateMappings' _ t@(LocationTree (_, Unmapped) _) =
      t & traversed .~ mempty
    propagateMappings' inheritedLayers (LocationTree (thisNode, MappedTo theseMappings) thisSub) =
      LocationTree thisNode' $ imap recur thisSub
      where
        theseLayers = applyMappingsToLayers inheritedLayers theseMappings
        thisNode' = case theseLayers of
          Nothing -> mempty
          Just tl -> f tl thisNode
        recur fname subtree = propagateMappings' sublayers subtree
          where
            sublayers =
              theseLayers & over (_Just.locLayers._1) (</> T.unpack (_ltpiName fname))

applyMappingsToLayers :: Maybe (LocLayers a) -> [(Maybe Loc, Maybe b)] -> Maybe (LocLayers (Maybe b))
applyMappingsToLayers inheritedLayers tm = layers
  where
    toLayers []     = Nothing
    toLayers (l:ls) = Just $ LocLayers l ls
    split (n, (Nothing, fmt))   = Left (n, fmt)
    split (n, (Just path, fmt)) = Right ((n,0::Int), (path, fmt))
    splitted = partitionEithers $ map split $ zip [(0::Int)..] tm
    layers = case (inheritedLayers, splitted) of
      (Nothing, (_, l)) -> toLayers $ map snd l
      (Just il, ([], [])) -> Just $
        il & traversed .~ Nothing
      (Just il, (justFmts, pathsAndFmts)) ->
        let justFmts' =
              [((order, order'), (loc, fmt))
              | (order', (loc, _)) <- zip [0..] (toListOf locLayers il)
              , (order, fmt)       <- justFmts]
        in toLayers $ map snd $
           sortBy (compare `on` fst) (justFmts' ++ pathsAndFmts)

-- | Transform a tree to one where unmapped nodes have been changed to 'mempty'
-- and mapped nodes have been associated to their physical 'Loc'. @n'@ is often
-- some file type or metadata that's required in the mapping.
applyMappings :: (Monoid n'')
              => (LocLayers (Maybe n') -> n -> n'')  -- ^ Add a physical location to a node
              -> LocationMappings n'  -- ^ Mappings to apply
              -> LocationTree n       -- ^ Original tree
              -> LocationTree n''  -- ^ Tree with physical locations
applyMappings f mappings loctree =
  propagateMappings f $
    insertMappings mappings loctree
