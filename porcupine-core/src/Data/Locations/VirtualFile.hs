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
  , DocRecOfOptions, RecOfOptions(..)
  , VFileImportance(..)
  , vfileSerials
  , vfileAsBidir, vfileImportance
  , vfileEmbeddedValue
  , setVFileIntermediaryValue, setVFileAesonValue
  , getVFileIntermediaryValue, getVFileAesonValue
  , getVFileRecOfOptions, setVFileRecOfOptionsLayers
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
      (SerialWriters toA toC)
      (SerialReaders fromA fromS fromC)
      prefExt
      _) = _vfileSerials vf
    intent
      | First (Just _) <- fromC, Just _ <- vf ^. vfileEmbeddedValue, First (Just _) <- toC = Just VFForCLIOptions
      | HM.null fromA && HM.null fromS && HM.null toA = Nothing
      | HM.null fromA && HM.null fromS = Just VFForWriting
      | HM.null toA = Just VFForReading
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

-- | If the 'VirtualFile' has an embedded value convertible to type @c@, we get
-- it.
getVFileIntermediaryValue :: forall a c. (Typeable c)
                          => VirtualFile a a
                          -> Maybe c
getVFileIntermediaryValue vf = let resTypeRep = typeOf (undefined :: c) in
  case HM.lookup (resTypeRep,Nothing)
                 (_serialWritersToAtomic $ _serialWriters $ _vfileSerials vf) of
    Nothing -> Nothing
    Just (ToAtomicFn toA) -> case cast (toA <$> vf ^. vfileEmbeddedValue) of
      Nothing -> error $ "getVFileIntermediaryValue: Impossible to cast the value to type "
        ++ show resTypeRep ++ ", however a conversion function was declared in the writers"
      Just i  -> i

-- | If the 'VirtualFile' has an embedded value that can be turned into an Aeson
-- 'Value', we get it.
getVFileAesonValue :: VirtualFile a a -> Maybe Value
getVFileAesonValue = getVFileIntermediaryValue

-- | If the 'VirtualFile' can hold a embedded value convertible from type @c@,
-- we set it (or remove it if Nothing). Note that the conversion may fail, we
-- return Left if the VirtualFile couldn't be set.
setVFileIntermediaryValue :: forall a b c. (Typeable c)
                          => VirtualFile a b
                          -> c
                          -> Either String (VirtualFile a b)
setVFileIntermediaryValue vf i = let inpTypeRep = typeOf (undefined :: c) in
  case HM.lookup (inpTypeRep,Nothing)
                 (_serialReadersFromAtomic $ _serialReaders $ _vfileSerials vf) of
    Nothing -> Left $ "No conversion function is available to turn type " ++ show inpTypeRep
               ++ "into the type expected by " ++ showVFileOriginalPath vf
    Just (FromAtomicFn fromA) -> case cast i of
      Nothing -> error $ "setVFileIntermediaryValue: Impossible to cast back the value from type "
        ++ show inpTypeRep ++ ", however a conversion function was declared in the readers"
      Just i' -> case fromA i' of
        Left s    -> Left s
        Right i'' -> Right $ vf & vfileEmbeddedValue .~ Just i''

-- | If the file can carry a defaut value convertible from aeson Value, we set
-- it.
setVFileAesonValue :: VirtualFile a b -> Value -> Either String (VirtualFile a b)
setVFileAesonValue = setVFileIntermediaryValue


-- * Compatibility layer with the doc records of options used by the
-- command-line parser

-- | Contains any set of options that should be exposed via the CLI
data RecOfOptions field where
  RecOfOptions :: (Typeable rs, RecordUsableWithCLI rs) => Rec field rs -> RecOfOptions field

type DocRecOfOptions = RecOfOptions DocField

-- | If the file has an embedded value that can be converted to a DocRecords, we
-- return it
getVFileRecOfOptions :: BidirVirtualFile a -> Maybe DocRecOfOptions
getVFileRecOfOptions vf = case mbopts of
  Just (defVal, WriteToConfigFn convert) -> Just $ RecOfOptions $ convert defVal
  where
    First mbopts = (,) <$> First (vf ^. vfileEmbeddedValue)
                       <*> vf ^. vfileSerials . serialWriters . serialWriterToConfig

-- | Assigns to the file a list of layers of records of options. No layers means
-- we remove the value. If we have more that one layer, this will fail if the
-- file doesn't use LayeredRead.
setVFileRecOfOptionsLayers :: VirtualFile a b -> [DocRecOfOptions] -> Either String (VirtualFile a b)
setVFileRecOfOptionsLayers vf layers =
  case vf ^. vfileSerials . serialReaders . serialReaderFromConfig of
    First Nothing -> Left $ showVFileOriginalPath vf ++
                      ": That file embedded value cannot be set from a DocRecOfOptions"
    First (Just (ReadFromConfigFn convert)) -> do
      let transform (RecOfOptions r) = case cast r of
            Nothing -> Left $ "setVFileRecOfOptionsLayers: " ++ showVFileOriginalPath vf
                              ++ ": record fields aren't compatible when they should"
            Just r' -> return $ convert r'
      newVal <- case (layers, vf^.vfileLayeredReadScheme) of
        ([], _) -> return Nothing
        ([x], _) -> Just <$> transform x
        ((l:ls), LayeredRead) -> Just . sconcat <$> traverse transform (l:|ls)
        (ls, LayeredReadWithNull) -> Just . mconcat <$> traverse transform ls
        (_, _) -> Left $ "setVFileRecOfOptionsLayers: " ++ showVFileOriginalPath vf
                  ++ ": that VirtualFile cannot use several layers of data"
      return $ vf & vfileEmbeddedValue .~ newVal

-- | Gives access to a version of the VirtualFile without type params. The
-- original path isn't settable.
vfileVoided :: Lens' (VirtualFile a b) (VirtualFile Void ())
vfileVoided f (VirtualFile p l v m i d s) =
  rebuild <$> f (VirtualFile p SingleLayerRead Nothing m i d mempty)
  where
    rebuild (VirtualFile _ _ _ m' i' d' _) =
      VirtualFile p l v m' i' d' s
