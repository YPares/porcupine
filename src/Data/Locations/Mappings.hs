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
  , HasDefaultMappingRule(..)
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


newtype LocationMappings_ n = LocationMappings_
  (HM.HashMap LocationTreePath (Mapping n))
  deriving (Functor, Show)

-- | Describes how physical locations are mapped to an application's
-- LocationTree. This is the type that is written to the pipeline yaml config
-- file under the "locations" section.
type LocationMappings n = LocationMappings_ (MbLocWithExt n)

instance Monoid (LocationMappings_ n) where
  mempty = LocationMappings_ mempty

instance Semigroup (LocationMappings_ n) where
  (LocationMappings_ m) <> (LocationMappings_ m') = LocationMappings_ $
    HM.unionWith f m m'
    where
      f Unmapped a                 = a
      f a Unmapped                 = a
      f (MappedTo l) (MappedTo l') = MappedTo (l ++ l')

instance (Representable n) => ToJSON (LocationMappings_ (MbLocWithExt n)) where
  toJSON (LocationMappings_ m) = Object $ HM.fromList $
    map (\(k, v) -> (toTextRepr k, toJSON v)) $ HM.toList m

instance (Representable n) => FromJSON (LocationMappings_ (MbLocWithExt n)) where
  parseJSON (Object m) = LocationMappings_ . HM.fromList <$>
    mapM (\(k, v) -> (,) <$> fromTextRepr k <*> parseJSON v) (HM.toList m)
  parseJSON _ = mempty

-- | Lists all the physical paths that have been associated to some virtual
-- location
allLocsInMappings :: LocationMappings_ (MbLocWithExt n) -> [Loc]
allLocsInMappings (LocationMappings_ m) =
  concat . mapMaybe (f . snd) $ HM.toList m
  where
    f Unmapped     = Nothing
    f (MappedTo a) = Just $ mapMaybe mbLocWithExt_Loc a

class HasDefaultMappingRule a where
  isMappedByDefault :: a -> Bool  -- ^ False if the resource by default should
                                  -- be mapped to 'null'

-- | Pre-fills the mappings from the context of a 'LocationTree', with extra
-- metadata saying whether each node should be explicitely mapped or unmapped.
mappingsFromLocTree :: (HasDefaultMappingRule a) => LocationTree a -> LocationMappings a
mappingsFromLocTree (LocationTree node (HM.toList -> [])) =
  LocationMappings_ $
    HM.fromList [(LTP []
                 , if isMappedByDefault node
                   then MappedTo [MbLocWithExt Nothing (Just node)]
                   else Unmapped)]
mappingsFromLocTree (LocationTree _ sub) =
  LocationMappings_ (mconcat $ map f $ HM.toList sub)
  where
    f (ltpi, t) =
      HM.fromList $ map appendPath $ HM.toList m
      where
        appendPath (LTP path, maps) = (LTP $ ltpi : path, maps)
        LocationMappings_ m = mappingsFromLocTree t

data MbLocWithExt n = MbLocWithExt { mbLocWithExt_Loc :: Maybe Loc, mbLocWithExt_Ext :: Maybe n }
  deriving (Show, Functor)

instance (Representable n) => ToJSON (MbLocWithExt n) where
  toJSON (MbLocWithExt Nothing Nothing)   = String "_"
  toJSON (MbLocWithExt Nothing (Just n))  = String $ case toTextRepr n of
    ""  -> "_"
    ext -> "_." <> ext
  toJSON (MbLocWithExt (Just l) Nothing)  = String $ toTextRepr l
  toJSON (MbLocWithExt (Just l) (Just n)) = Object $ HM.fromList
    [(toTextRepr l, String $ toTextRepr n)]


-- | If a ressource is mapped, it's mapped to a series of _layers_, each
-- subsequent layer having the capacity to override the previous ones. When a
-- layer's Loc is 'Nothing', then it means that this layer's path should be
-- inherited from layers up the tree.
data Mapping n = Unmapped | MappedTo [n]
  deriving (Eq, Show, Functor, Generic)

instance (ToJSON n) => ToJSON (Mapping n) where
  toJSON locs = case locs of
    Unmapped      -> Null
    MappedTo [ln] -> toJSON ln
    MappedTo p    -> toJSON $ map toJSON p

instance (Representable a) => FromJSON (Mapping (MbLocWithExt a)) where
  parseJSON Null                           = pure Unmapped
  parseJSON (String (T.toLower -> "none")) = pure Unmapped
  parseJSON j = MappedTo <$> readPathAndFormat j
    where
      -- The underscore sign here means "reuse inherited", depending on the
      -- position it can mean either file path or extension or both.
      readPathAndFormat (String "_") = pure
        [MbLocWithExt Nothing Nothing]
      readPathAndFormat (String s) =
        case splitExtension $ T.unpack s of
          (path, "") -> pure [MbLocWithExt (fromTextRepr $ T.pack path) Nothing]
          (path, ext) ->
            let ext' = case ext of
                  '.' : e -> e
                  e       -> e
                path' = case path of
                  "_" -> Nothing
                  _   -> fromTextRepr $ T.pack path
            in map (MbLocWithExt path') <$> case ext' of
              "_" -> pure [Nothing]
              _   -> parseSerMethList (String $ T.pack ext')
      readPathAndFormat (Object (HM.toList -> [(l, fmts)])) = do
        -- checking that we are matching for a single key based object
        fmts' <- parseSerMethList fmts
        case (fmts', null $ snd $ splitExtension $ T.unpack l) of
          (_:_:_, False) -> fail $ "'" ++ T.unpack l ++
            "' is invalid: a file cannot contain an extension when mapped to several types"
          _ -> return $ map (MbLocWithExt (fromTextRepr l)) fmts'
      readPathAndFormat (Array m)    = concat <$>
        mapM readPathAndFormat (toListOf traversed m)
      readPathAndFormat _            = fail
        "A location mapping must either be a map of one 'path: format' entry,\
        \ be just a 'format' string or be an array of some of these."

parseSerMethList :: (Representable a, Monad m) => Value -> m [Maybe a]
parseSerMethList Null         = pure [fromTextRepr ""]
parseSerMethList (String fmt) = case fromTextRepr fmt of
  Just f  -> pure [Just f]
  Nothing -> fail $ "Unhandled serialization method: " ++ T.unpack fmt
parseSerMethList (Object (HM.toList -> [(_fmt, Object _reprOpts)])) = fail
  "Serialization options for formats aren't implemented yet"
  -- TODO: implement options (pretty printing etc.) for serialization methods
parseSerMethList (Array fmts) = concat <$>
  mapM parseSerMethList (toListOf traversed fmts)
parseSerMethList _            = fail
  "Format must be a string, a null or an array of strings"

-- | Just packs a value with a Bool indicating whether by default, this value
-- should be used or not.
data WithDefaultUsage a = WithDefaultUsage Bool a

instance HasDefaultMappingRule (WithDefaultUsage a) where
  isMappedByDefault (WithDefaultUsage b _) = b


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

-- | Creates a 'LocationMappings_' where the whole LocationTree is mapped to a
-- single folder
mappingRootOnly :: (Default a) => Loc -> LocationMappings_ (MbLocWithExt a)
mappingRootOnly l = LocationMappings_ $ HM.fromList [(LTP [], MappedTo [MbLocWithExt (Just l) def])]

-- | Gets the last layer folder mapped to the root. Returns Nothing if the root
-- has several mappings.
getLocMappedToRoot :: LocationMappings_ (MbLocWithExt n) -> Maybe Loc
getLocMappedToRoot (LocationMappings_ h) = case HM.lookup (LTP []) h of
  Just (MappedTo (reverse -> MbLocWithExt (Just l) _:_)) -> Just l
  _                                                      -> Nothing

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

-- | For each location in the tree, gives it a final, physical location or
-- just put Nothing if it isn't mapped
propagateMappings :: (Monoid n'')
                  => (LocLayers (Maybe n') -> n -> n'')
                  -> LocationTree (n, Mapping (MbLocWithExt n'))
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

applyMappingsToLayers :: Maybe (LocLayers a) -> [MbLocWithExt b] -> Maybe (LocLayers (Maybe b))
applyMappingsToLayers inheritedLayers tm = layers
  where
    toLayers []     = Nothing
    toLayers (l:ls) = Just $ LocLayers l ls
    split (n, (MbLocWithExt Nothing     fmt)) = Left (n, fmt)
    split (n, (MbLocWithExt (Just path) fmt)) = Right ((n,0::Int), (path, fmt))
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
