{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Locations.VirtualFile
  ( LocationTreePathItem
  , module Data.Locations.SerializationMethod
  , Void
  , Profunctor(..)
  , VirtualFile(..), LayeredReadScheme(..)
  , BidirVirtualFile, DataSource, DataSink
  , VirtualFileIntent(..), VirtualFileDescription(..)
  , RecOfOptions(..)
  , VFileImportance(..)
  , vfileSerials
  , vfileAsBidir, vfileImportance
  , vfileEmbeddedValue
  , getConvertedEmbeddedValue, setConvertedEmbeddedValue
  , tryMergeLayersForVFile
  , vfileOriginalPath, showVFileOriginalPath
  , vfileLayeredReadScheme
  , vfileVoided
  , vfiReadSuccess, vfiWriteSuccess, vfiError
  , dataSource, dataSink, bidirVirtualFile
  , makeSink, makeSource
  , documentedFile
  , withEmbeddedValue
  , usesLayeredMapping, canBeUnmapped, unmappedByDefault
  , getVirtualFileDescription
  ) where

import           Control.Lens
import           Data.Aeson                         (Value)
import           Data.Default
import           Data.DocRecord
import           Data.DocRecord.OptParse            (RecordUsableWithCLI)
import qualified Data.HashMap.Strict                as HM
import qualified Data.HashSet                       as HS
import           Data.List                          (intersperse)
import           Data.List.NonEmpty                 (NonEmpty (..))
import           Data.Locations.Loc
import           Data.Locations.LocationTree
import           Data.Locations.Mappings            (HasDefaultMappingRule (..),
                                                     LocShortcut (..))
import           Data.Locations.SerializationMethod
import           Data.Maybe
import           Data.Monoid                        (First (..))
import           Data.Profunctor                    (Profunctor (..))
import           Data.Representable
import           Data.Semigroup                     (sconcat)
import qualified Data.Text                          as T
import           Data.Type.Equality
import           Data.Typeable
import           Data.Void
import           Katip


-- * The general 'VirtualFile' type

-- | Tells how the file is meant to be read
data LayeredReadScheme b where
  SingleLayerRead     :: LayeredReadScheme b
    -- No layered reading accepted
  LayeredRead         :: Semigroup b => LayeredReadScheme b
    -- A layered reading combining all the layers with (<>)
  LayeredReadWithNull :: Monoid b => LayeredReadScheme b
    -- Like 'LayeredRead', and handles mapping to no layer (mempty)

-- | Tells how the accesses to this 'VirtualFile' should be logged
data VFileImportance = VFileImportance
  { _vfiReadSuccess  :: Severity
  , _vfiWriteSuccess :: Severity
  , _vfiError        :: Severity }

makeLenses ''VFileImportance

instance Default VFileImportance where
  def = VFileImportance DebugS NoticeS ErrorS

-- | A virtual file in the location tree to which we can write @a@ and from
-- which we can read @b@.
data VirtualFile a b = VirtualFile
  { _vfileOriginalPath      :: [LocationTreePathItem]
  , _vfileLayeredReadScheme :: LayeredReadScheme b
  , _vfileEmbeddedValue     :: Maybe b
  , _vfileMappedByDefault   :: Bool
  , _vfileImportance        :: VFileImportance
  , _vfileDocumentation     :: Maybe T.Text
  , _vfileSerials           :: SerialsFor a b }

makeLenses ''VirtualFile

-- How we derive the default configuration for mapping some VirtualFile
instance HasDefaultMappingRule (VirtualFile a b) where
  getDefaultLocShortcut vf = if vf ^. vfileMappedByDefault
    then Just $
      case vf ^? vfileSerials . serialRepetitionKeys . filtered (not . null) of
        Nothing -> DeriveWholeLocFromTree defExt
        -- LIMITATION: For now we suppose that every reading/writing function in
        -- the serials has the same repetition keys
        Just rkeys -> DeriveLocPrefixFromTree $
          let toVar rkey = SWVB_VarRef rkey
              locStr = StringWithVars $ (SWVB_Chunk "-")
                       : intersperse (SWVB_Chunk "-") (map toVar rkeys)
          in LocFilePath locStr $ T.unpack defExt
    else Nothing
    where
      defExt =
        case vf ^. vfileSerials . serialDefaultExt of
          First (Just ext) -> ext
          _                -> T.pack ""

-- For now, given the requirement of PTask, VirtualFile has to be a Monoid
-- because a Resource Tree also has to.
instance Semigroup (VirtualFile a b) where
  VirtualFile p l v m i d s <> VirtualFile _ _ _ _ _ _ s' =
    VirtualFile p l v m i d (s<>s')
instance Monoid (VirtualFile a b) where
  mempty = VirtualFile [] SingleLayerRead Nothing True def Nothing mempty

instance Profunctor VirtualFile where
  dimap f g (VirtualFile p _ v m i d s) =
    VirtualFile p SingleLayerRead (g <$> v) m i d $ dimap f g s


-- * Obtaining a description of how the 'VirtualFile' should be used

-- | Describes how a virtual file is meant to be used
data VirtualFileIntent =
  VFForWriting | VFForReading | VFForRW | VFForCLIOptions
  deriving (Show, Eq)

-- | Gives the purpose of the 'VirtualFile'. Used to document the pipeline and check
-- mappings to physical files.
data VirtualFileDescription = VirtualFileDescription
  { vfileDescIntent             :: Maybe VirtualFileIntent
                        -- ^ How is the 'VirtualFile' meant to be used
  , vfileDescEmbeddableInConfig :: Bool
                        -- ^ True if the data can be read directly from the
                        -- pipeline's config file
  , vfileDescEmbeddableInOutput :: Bool
                        -- ^ True if the data can be written directly in the
                        -- pipeline's output location tree
  , vfileDescPossibleExtensions :: [FileExt]
                        -- ^ Possible extensions for the files this virtual file
                        -- can be mapped to (prefered extension is the first)
  }
  deriving (Show)

-- | Gives a 'VirtualFileDescription'. To be used on files stored in the
-- ResourceTree.
getVirtualFileDescription :: VirtualFile a b -> VirtualFileDescription
getVirtualFileDescription vf =
  VirtualFileDescription intent readableFromConfig writableInOutput exts
  where
    (SerialsFor
      (SerialWriters toA)
      (SerialReaders fromA fromS)
      prefExt
      _) = _vfileSerials vf
    intent
      | HM.null fromA && HM.null fromS && HM.null toA = Nothing
      | HM.null fromA && HM.null fromS = Just VFForWriting
      | HM.null toA = Just VFForReading
      | Just _ <- vf ^. vfileEmbeddedValue = Just VFForCLIOptions
      | otherwise = Just VFForRW
    extSet = HS.fromList . mapMaybe snd . HM.keys
    otherExts = extSet toA <> extSet fromA <> extSet fromS
    exts = case prefExt of
             First (Just e) -> e:(HS.toList $ HS.delete e otherExts)
             _              -> HS.toList otherExts
    typeOfAesonVal = typeOf (undefined :: Value)
    readableFromConfig = (typeOfAesonVal,Nothing) `HM.member` fromA
    writableInOutput = (typeOfAesonVal,Nothing) `HM.member` toA

-- | Just for logs and error messages
showVFileOriginalPath :: VirtualFile a b -> String
showVFileOriginalPath = T.unpack . toTextRepr .  LTP . _vfileOriginalPath

-- | Embeds a value inside the 'VirtualFile'. This value will be considered the
-- base layer if we read extra @b@'s from external physical files.
withEmbeddedValue :: b -> VirtualFile a b -> VirtualFile a b
withEmbeddedValue = set vfileEmbeddedValue . Just

-- | Indicates that the file uses layered mapping
usesLayeredMapping :: (Semigroup b) => VirtualFile a b -> VirtualFile a b
usesLayeredMapping =
  vfileLayeredReadScheme .~ LayeredRead

-- | Indicates that the file uses layered mapping, and additionally can be left
-- unmapped (ie. mapped to null)
canBeUnmapped :: (Monoid b) => VirtualFile a b -> VirtualFile a b
canBeUnmapped =
  vfileLayeredReadScheme .~ LayeredReadWithNull

-- | Indicates that the file should be mapped to null by default
unmappedByDefault :: (Monoid b) => VirtualFile a b -> VirtualFile a b
unmappedByDefault =
    (vfileLayeredReadScheme .~ LayeredReadWithNull)
  . (vfileMappedByDefault .~ False)

-- | Gives a documentation to the 'VirtualFile'
documentedFile :: T.Text -> VirtualFile a b -> VirtualFile a b
documentedFile doc = vfileDocumentation .~ Just doc


-- * Creating VirtualFiles and convertings between its different subtypes (bidir
-- files, sources and sinks)

-- | A virtual file which depending on the situation can be written or read
type BidirVirtualFile a = VirtualFile a a

-- | A virtual file that's only readable
type DataSource a = VirtualFile Void a

-- | A virtual file that's only writable
type DataSink a = VirtualFile a ()

-- | Creates a virtuel file from its virtual path and ways serialize/deserialize
-- the data. You should prefer 'dataSink' and 'dataSource' for clarity when the
-- file is meant to be readonly or writeonly.
virtualFile :: [LocationTreePathItem] -> SerialsFor a b -> VirtualFile a b
virtualFile path sers = VirtualFile path SingleLayerRead Nothing True def Nothing sers

-- | Creates a virtual file from its virtual path and ways to deserialize the
-- data.
dataSource :: [LocationTreePathItem] -> SerialsFor a b -> DataSource b
dataSource path = makeSource . virtualFile path

-- | Creates a virtual file from its virtual path and ways to serialize the
-- data.
dataSink :: [LocationTreePathItem] -> SerialsFor a b -> DataSink a
dataSink path = makeSink . virtualFile path

-- | Like 'virtualFile', but constrained to bidirectional serials, for clarity
bidirVirtualFile :: [LocationTreePathItem] -> BidirSerials a -> BidirVirtualFile a
bidirVirtualFile = virtualFile

-- | Turns the 'VirtualFile' into a pure sink
makeSink :: VirtualFile a b -> DataSink a
makeSink vf = vf{_vfileSerials=eraseDeserials $ _vfileSerials vf
                ,_vfileLayeredReadScheme=LayeredReadWithNull
                ,_vfileEmbeddedValue=Nothing}

-- | Turns the 'VirtualFile' into a pure source
makeSource :: VirtualFile a b -> DataSource b
makeSource vf = vf{_vfileSerials=eraseSerials $ _vfileSerials vf}


-- * Traversals to the content of the VirtualFile, when it already embeds some
-- value

-- | If we have the internal proof that a VirtualFile is actually bidirectional,
  -- we convert it.
vfileAsBidir :: forall a b. (Typeable a, Typeable b)
             => Traversal' (VirtualFile a b) (BidirVirtualFile a)
vfileAsBidir f vf = case eqT :: Maybe (a :~: b) of
  Just Refl -> f vf
  Nothing   -> pure vf

-- | Gives access to a version of the VirtualFile without type params. The
-- original path isn't settable.
vfileVoided :: Lens' (VirtualFile a b) (VirtualFile Void ())
vfileVoided f (VirtualFile p l v m i d s) =
  rebuild <$> f (VirtualFile p SingleLayerRead Nothing m i d mempty)
  where
    rebuild (VirtualFile _ _ _ m' i' d' _) =
      VirtualFile p l v m' i' d' s

-- | If the 'VirtualFile' has an embedded value convertible to type @i@, we get
-- it.
getConvertedEmbeddedValue
  :: (Typeable i)
  => BidirVirtualFile a
  -> Maybe i
getConvertedEmbeddedValue vf = do
  toA <- getToAtomicFn (vf ^. vfileSerials)
  toA <$> vf ^. vfileEmbeddedValue

-- | If the 'VirtualFile' can hold a embedded value convertible from type @i@,
-- we set it (or remove it if Nothing). Note that the conversion may fail, we
-- return Left if the VirtualFile couldn't be set.
setConvertedEmbeddedValue
  :: forall a b i. (Typeable i)
  => VirtualFile a b
  -> i
  -> Either String (VirtualFile a b)
setConvertedEmbeddedValue vf i =
  case getFromAtomicFn (vf ^. vfileSerials) of
    Nothing -> Left $ showVFileOriginalPath vf ++
               ": no conversion function is available to transform type " ++ show (typeOf (undefined :: i))
    Just fromA -> do
      i' <- fromA i
      return $ vf & vfileEmbeddedValue .~ Just i'

-- | Tries to convert each @i@ layer to and from type @b@ and find a
-- Monoid/Semigroup instance for @b@ in the vfileLayeredReadScheme, so we can
-- merge these layers. So if we have more that one layer, this will fail if the
-- file doesn't use LayeredRead.
tryMergeLayersForVFile
  :: forall a i. (Typeable i)
  => BidirVirtualFile a
  -> [i]
  -> Either String i
tryMergeLayersForVFile _ [i] = return i
tryMergeLayersForVFile vf layers = let ser = vf ^. vfileSerials in
  case (,) <$> getFromAtomicFn ser <*> getToAtomicFn ser of
    Nothing -> Left $ showVFileOriginalPath vf ++
               ": no conversion functions are available to transform back and forth type "
               ++ show (typeOf (undefined :: i))
    Just (fromA, toA) -> do
      newVal <- case (layers, vf^.vfileLayeredReadScheme) of
        ([], LayeredReadWithNull) -> return mempty
        ([], _) -> Left $ "tryMergeLayersForVFile: " ++ showVFileOriginalPath vf
                   ++ " doesn't support mapping to no layers"
        ([x], _) -> error "tryMergeLayersForVFile: Should have been handled by now"
        (l:ls, LayeredRead) -> sconcat <$> traverse fromA (l:|ls)
        (ls, LayeredReadWithNull) -> mconcat <$> traverse fromA ls
        (_, _) -> Left $ "tryMergeLayersForVFile: " ++ showVFileOriginalPath vf
                  ++ " cannot use several layers of data"
      return $ toA newVal
