{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds            #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Abstraction layer around the filesystem, so that inputs and outputs may be
-- redirected to various files without the logic code having to know.
module Data.Locations.LocationTree
  (
  -- * Types
    LocationTree(..)
  , LocationTreePathItem(..), LocationTreePath(..)
  , LTPIAndSubtree(..)
  , (:||)(..), _Unprioritized, _Prioritized
  -- * Functions
  , locTreeNodeTag, locTreeSubfolders
  , inLocTree
  , allSubLocTrees, traversedTreeWithPath
  , atSubfolder, atSubfolderRec
  , filteredLocsInTree
  , subtractPathFromTree
  , singLTP
  , showLTPIName
  , ltpiName
  , addExtToLocIfMissing
  , locNode, folderNode, fileEmpty, file
  , splitLocTree, joinLocTrees
  , locTreeToDataTree
  , prettyLocTree
  , apLocationTree
  )
where

import           Control.Applicative
import           Control.Lens        hiding ((<.>))
import           Data.Aeson
import           Data.Binary
import           Data.Hashable
import qualified Data.HashMap.Strict as HM
import           Data.List
import           Data.Locations.Loc
import           Data.Maybe
import           Data.Representable
import           Data.String
import qualified Data.Text           as T
import qualified Data.Tree           as DT
import           GHC.Generics        (Generic)
-- import Data.Tree.Pretty
-- import Diagrams.TwoD.Layout.Tree


-- | A very simple virtual filesystem. Defines a hierarchy of virtual locations,
-- and some rules for how to store and read files. Each type of pipeline
-- (solving, exploration) will need its 'LocationTree', that can be obtained by
-- composing the 'LocationTree's of the tasks it contains.
--
-- In a project using pipeline-tools, the project's code will only use virtual
-- paths, and it won't know what actual physical location is behind that path.
--
-- We could use a DocRecord to represent a LocationTree. Maybe we'll refactor it
-- to use DocRecords in the future, but for now it was simpler to use a simple
-- tree of maps.
data LocationTree a = LocationTree
  { _locTreeNodeTag    :: a -- ^ In the case of a 'BareLocationTree', indicates
                            -- the prefered serialization method of the content
                            -- of that node. If that node is a folder, the
                            -- serialization method can mean that its content
                            -- will be packed at a single place (for instance
                            -- one table in a database to group several virtual
                            -- JSON files)
  , _locTreeSubfolders :: HM.HashMap LocationTreePathItem (LocationTree a)
                  -- ^ The content of the node. Is empty for a terminal file.
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance (Monoid a) => Monoid (LocationTree a) where
  mempty = LocationTree mempty mempty

instance (Semigroup a) => Semigroup (LocationTree a) where
  LocationTree m1 s1 <> LocationTree m2 s2 =
    LocationTree (m1 <> m2) (HM.unionWith (<>) s1 s2)

-- | LocationTree cannot be an applicative because pure cannot construct an
-- infinite tree (since HashMaps are strict in their keys), but <*> can be
-- implemented, and a LocationTree is already a Functor. Branches that don't
-- match are just abandoned.
apLocationTree :: LocationTree (a -> b) -> LocationTree a -> LocationTree b
apLocationTree (LocationTree f sub) (LocationTree x sub') = LocationTree (f x) sub''
  where
    sub'' = HM.intersectionWith apLocationTree sub sub'

-- | Identifies a folder or file-like object in the 'LocationTree'.
data LocationTreePathItem
  = LTPI { _ltpiName         :: T.Text -- ^ Name of the file or folder
         -- , _ltpiIsRepeatable :: Bool  -- ^ If true, then will correspond to
         --                             -- several files or folders, where _ltpiName
         --                             -- is just a prefix followed by a number
         --                             -- (e.g. patient01, patient02, etc.)
         -- , _ltpiIsTerminal   :: Bool -- ^ If true, is a pure file that must be
         --                          -- deserizalized as a whole
         }
  deriving (Eq, Ord, Show, Generic, Hashable, Binary)

showLTPIName :: LocationTreePathItem -> String
showLTPIName = T.unpack . _ltpiName

singleFolder :: T.Text -> LocationTreePathItem
singleFolder x = LTPI x -- False False

instance IsString LocationTreePathItem where
  fromString = singleFolder . T.pack

-- | A path in a 'LocationTree'
newtype LocationTreePath = LTP [LocationTreePathItem]
  deriving (Eq, Ord, Show, Generic, Hashable, Binary)

instance Representable LocationTreePath where
  toTextRepr (LTP l) = mconcat $ "/" : intersperse "/" (map _ltpiName l)
  fromTextRepr =
    pure . LTP . map singleFolder . filter (not . T.null) . T.splitOn "/"

instance ToJSON LocationTreePath where
  toJSON = String . toTextRepr

instance FromJSON LocationTreePath where
  parseJSON (String t) = fromTextRepr t
  parseJSON _          = mempty

instance ToJSONKey LocationTreePath
instance FromJSONKey LocationTreePath

instance Semigroup LocationTreePath where
  (LTP a) <> (LTP b) = LTP $ a ++ b
instance Monoid LocationTreePath where
  mempty = LTP []

singLTP :: LocationTreePathItem -> LocationTreePath
singLTP = LTP . (:[])

-- | Permits to filter a tree and to remove some nodes
filteredLocsInTree
  :: Traversal (LocationTree a) (Maybe (LocationTree b)) a (Maybe b)
filteredLocsInTree f (LocationTree a sub) =
  liftA2 LocationTree <$> f a
    <*> (Just . HM.fromList . catMaybes <$> traverse onSub (HM.toList sub))
  where
    onSub (k,t) = fmap (k,) <$> filteredLocsInTree f t

-- | Access or edit a subtree
inLocTree :: LocationTreePath -> Lens' (LocationTree a) (Maybe (LocationTree a))
inLocTree path f t = fromJust <$> go path (Just t)
  where
    go _        Nothing = f Nothing
    go (LTP []) mbT     = f mbT
    go (LTP (p:ps)) (Just (LocationTree m s)) = rebuild <$> go (LTP ps) (HM.lookup p s)
      where
        rebuild Nothing | HM.null s' = Nothing
                        | otherwise  = Just $ LocationTree m s'
          where s' = HM.delete p s
        rebuild (Just res) = Just $ LocationTree m $ HM.insert p res s

-- | Find all the subtrees, indexed by their 'LocationTreePath'
allSubLocTrees
  :: Traversal (LocationTree a) (LocationTree b)
               (LocationTreePath, LocationTree a) b
allSubLocTrees f = go []
  where go ps n@(LocationTree _ sub) = LocationTree
          <$> f (LTP $ reverse ps, n)
          <*> itraverse (\p n' -> go (p:ps) n') sub

-- | Traverse all the nodes, indexed by their 'LocationTreePath'
traversedTreeWithPath
  :: Traversal (LocationTree a) (LocationTree b)
               (LocationTreePath, a) b
traversedTreeWithPath f = go []
  where go ps (LocationTree n sub) = LocationTree
          <$> f (LTP $ reverse ps, n)
          <*> itraverse (\p n' -> go (p:ps) n') sub

-- | Removes a path from a 'LocationTree'.
subtractPathFromTree :: LocationTree a -> LocationTreePath -> LocationTree a
subtractPathFromTree tree path = tree & inLocTree path .~ Nothing

-- | Just a tuple-like type. An entry for the map of contents at some path in a
-- 'LocationTree'
data LTPIAndSubtree a = LocationTreePathItem :/ LocationTree a
  deriving (Eq, Show, Functor, Foldable, Traversable)

infixr 5 :/

locNode :: a -> [LTPIAndSubtree a] -> LocationTree a
locNode a = LocationTree a . HM.fromList . map (\(x:/y) -> (x,y))

-- | A shortcut for 'locNode' for folders
folderNode :: (Monoid a) => [LTPIAndSubtree a] -> LocationTree a
folderNode = locNode mempty

fileEmpty :: (Monoid a)
     => LocationTreePathItem
     -> LTPIAndSubtree a
fileEmpty i = i :/ mempty
file :: LocationTreePathItem
      -> a
      -> LTPIAndSubtree a
file i a = i :/ LocationTree a mempty

instance (Monoid a) => IsString (LTPIAndSubtree a) where
  fromString = fileEmpty . fromString

addExtToLocIfMissing :: Loc -> T.Text -> Loc
addExtToLocIfMissing loc ext | T.null (loc^.locExt) =
  if T.null ext
     then loc
     else loc & locExt .~ ext
addExtToLocIfMissing loc _ = loc

-- | Like Either, but equipped with a Monoid instance that would prioritize Right over Left
data a :|| b = Unprioritized a | Prioritized b
infixr 5 :||

instance (Semigroup a, Semigroup b) => Semigroup (a :|| b) where
  (<>) (Prioritized x) (Prioritized x')     = Prioritized (x<>x')
  (<>) (Unprioritized x) (Unprioritized x') = Unprioritized (x<>x')
  (<>) p@(Prioritized _) _                  = p
  (<>) _ p@(Prioritized _)                  = p
instance (Monoid a, Monoid b) => Monoid (a :|| b) where
  mempty = Unprioritized mempty

-- | Merges two trees of different node types, prioritizing those of the second
-- tree when a node exists in both trees
joinLocTrees
  :: (Monoid a, Monoid b)
  => LocationTree a -> LocationTree b -> LocationTree (a :|| b)
joinLocTrees ta tb = fmap Unprioritized ta <> fmap Prioritized tb

-- | Splits a 'LocationTree' of @a :|| b@ into two trees that will have the same
-- structure but not the same nodes
splitLocTree :: LocationTree (a :|| b) -> (LocationTree (Maybe a), LocationTree (Maybe b))
splitLocTree (LocationTree n sub) = case n of
  Unprioritized a -> (LocationTree (Just a) subA, LocationTree Nothing subB)
  Prioritized b   -> (LocationTree Nothing  subA, LocationTree (Just b) subB)
  where
    subA = HM.fromList subAL
    subB = HM.fromList subBL
    (subAL, subBL) = unzip $
      map (\(path, ltree) ->
             let (na, nb) = splitLocTree ltree
             in ((path, na), (path, nb)))
          (HM.toList sub)

makeLenses ''LocationTree
makeLenses ''LocationTreePathItem
makePrisms ''(:||)


atSubfolder :: Applicative f => LocationTreePathItem -> (LocationTree a -> f (LocationTree a)) -> LocationTree a -> f (LocationTree a)
atSubfolder pathItem = locTreeSubfolders . at pathItem . _Just

atSubfolderRec :: (Applicative f, Foldable t) => t LocationTreePathItem -> (LocationTree a -> f (LocationTree a)) -> LocationTree a -> f (LocationTree a)
atSubfolderRec path =
  foldr (\pathItem subtree -> atSubfolder pathItem . subtree) id path

locTreeToDataTree :: LocationTree b -> DT.Tree (LocationTreePathItem, b)
locTreeToDataTree t = toCanonicalTree "/" t
  where
    toCanonicalTree p (LocationTree n sub) =
      DT.Node (p,n) $ map (uncurry toCanonicalTree) $ HM.toList sub

prettyLocTree :: (Show a) => LocationTree a -> String
prettyLocTree t = DT.drawTree t'
  where
    str (p,n) = T.unpack (_ltpiName p) ++ ": " ++ show n
    t' = fmap str $ locTreeToDataTree t
