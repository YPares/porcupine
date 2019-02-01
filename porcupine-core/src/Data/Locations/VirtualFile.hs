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
  , vfileBidirProof, vfileSerials
  , vfileAsBidir, vfileAsBidirE, vfileImportance
  , vfileEmbeddedValue, vfileIntermediaryValue, vfileAesonValue
  , vfileOriginalPath, showVFileOriginalPath
  , vfileLayeredReadScheme
  , vfileVoided
  , vfiReadSuccess, vfiWriteSuccess, vfiError
  , dataSource, dataSink, bidirVirtualFile, ensureBidirFile
  , makeSink, makeSource
  , documentedFile
  , usesLayeredMapping, canBeUnmapped, unmappedByDefault
  , getVirtualFileDescription
  , vfileRecOfOptions
  ) where

import           Control.Lens
import           Data.Aeson                         (Value)
import           Data.Default
import           Data.DocRecord
import           Data.DocRecord.OptParse            (RecordUsableWithCLI)
import           Data.Functor.Compose
import qualified Data.HashMap.Strict                as HM
import qualified Data.HashSet                       as HS
import           Data.List                          (intersperse)
import           Data.Locations.Loc
import           Data.Locations.LocationTree
import           Data.Locations.Mappings            (HasDefaultMappingRule (..),
                                                     LocShortcut (..))
import           Data.Locations.SerializationMethod
import           Data.Maybe
import           Data.Monoid                        (First (..))
import           Data.Profunctor                    (Profunctor (..))
import           Data.Representable
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
  , _vfileMappedByDefault   :: Bool
  , _vfileImportance        :: VFileImportance
  , _vfileDocumentation     :: Maybe T.Text
  , _vfileBidirProof        :: Maybe (a :~: b)
                    -- Temporary, necessary until we can do away with docrec
                    -- conversion in the writer part of SerialsFor
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
-- TODO: explain the meaning of (<>). From the implementation,
-- computing the union of serials and ignoring the location seems pretty
-- arbitrary.
instance Semigroup (VirtualFile a b) where
  VirtualFile p u m i d b s <> VirtualFile _ _ _ _ _ _ s' =
    VirtualFile p u m i d b (s<>s')
-- TODO: It doesn't look like this instance satisfies the identity
-- laws of Monoid. Comment here what's the plan to address this?
instance Monoid (VirtualFile a b) where
  mempty = VirtualFile [] SingleLayerRead True def Nothing Nothing mempty

instance Profunctor VirtualFile where
  -- TODO: explain why the LayeredReadScheme of the input is ignored?
  dimap f g (VirtualFile p _ m i d _ s) =
    VirtualFile p SingleLayerRead m i d Nothing $ dimap f g s


-- * Obtaining a description of how the 'VirtualFile' should be used

-- | Describes how a virtual file is meant to be used
data VirtualFileIntent =
  VFForWriting | VFForReading | VFForRW | VFForCaching | VFForCLIOptions
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
      (SerialReaders fromA fromS fromC fromV)
      prefExt
      _) = _vfileSerials vf
    intent
      | First (Just _) <- fromC, First (Just _) <- fromV, First (Just _) <- toC = Just VFForCLIOptions
      | HM.null fromA && HM.null fromS && HM.null toA = Nothing
      | HM.null fromA && HM.null fromS = Just VFForWriting
      | HM.null toA = Just VFForReading
      | (Just _) <- _vfileBidirProof vf = Just VFForCaching
      | otherwise = Just VFForRW
    extSet = HS.fromList . catMaybes . map snd . HM.keys
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

-- | Indicates that the file uses layered mapping
-- TODO: put it in imperative form: useLayeredMapping
usesLayeredMapping :: (Semigroup b) => VirtualFile a b -> VirtualFile a b
usesLayeredMapping =
  vfileLayeredReadScheme .~ LayeredRead

-- | Indicates that the file uses layered mapping, and additionally can be left
-- unmapped (ie. mapped to null)
-- TODO: put it in imperative form: allowToBeUnmapped
canBeUnmapped :: (Monoid b) => VirtualFile a b -> VirtualFile a b
canBeUnmapped =
  vfileLayeredReadScheme .~ LayeredReadWithNull

-- | Indicates that the file should be mapped to null by default
-- TODO: put it in imperative form: unmapByDefault
unmappedByDefault :: (Monoid b) => VirtualFile a b -> VirtualFile a b
unmappedByDefault =
    (vfileLayeredReadScheme .~ LayeredReadWithNull)
  . (vfileMappedByDefault .~ False)

-- | Gives a documentation to the 'VirtualFile'
-- TODO: put it in imperative form: documentFile
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
virtualFile :: [LocationTreePathItem] -> Maybe (a :~: b) -> SerialsFor a b -> VirtualFile a b
virtualFile path refl sers = VirtualFile path SingleLayerRead True def Nothing refl sers

-- | Creates a virtual file from its virtual path and ways to deserialize the
-- data.
dataSource :: [LocationTreePathItem] -> SerialsFor a b -> DataSource b
dataSource path = makeSource . virtualFile path Nothing

-- | Creates a virtual file from its virtual path and ways to serialize the
-- data.
dataSink :: [LocationTreePathItem] -> SerialsFor a b -> DataSink a
dataSink path = makeSink . virtualFile path Nothing

-- | Like VirtualFile, except we will embed the proof that @a@ and @b@ are the same
bidirVirtualFile :: [LocationTreePathItem] -> BidirSerials a -> BidirVirtualFile a
bidirVirtualFile path sers = virtualFile path (Just Refl) sers

-- | If a file has been transformed via Profunctor methods (dimap, rmap, rmap),
-- even if it remained bidirectional this proof it is has been lost. Thus we
-- have to embed it again.
-- TODO: Get rid of this somehow?
ensureBidirFile :: VirtualFile a a -> VirtualFile a a
ensureBidirFile vf = vf{_vfileBidirProof=Just Refl}

makeSink :: VirtualFile a b -> DataSink a
makeSink vf = vf{_vfileSerials=eraseDeserials $ _vfileSerials vf
                ,_vfileBidirProof=Nothing
                ,_vfileLayeredReadScheme=LayeredReadWithNull}

makeSource :: VirtualFile a b -> DataSource b
makeSource vf = vf{_vfileSerials=eraseSerials $ _vfileSerials vf
                  ,_vfileBidirProof=Nothing}


-- * Traversals to the content of the VirtualFile, when it already embeds some
-- value

-- | If we have the internal proof that a VirtualFile is actually bidirectional,
-- we convert it.
vfileAsBidir :: Traversal' (VirtualFile a b) (VirtualFile a a)
vfileAsBidir f vf = case _vfileBidirProof vf of
  Just Refl -> f vf
  Nothing   -> pure vf

-- | Like 'vfileAsBidir', but can be composed with other traversals that can
-- fail, like 'vfileIntermediaryValue'
vfileAsBidirE :: Traversal' (Either s (VirtualFile a b)) (Either s (VirtualFile a a))
vfileAsBidirE _ (Left s) = pure (Left s)
vfileAsBidirE f (Right vf) = case _vfileBidirProof vf of
  Just Refl -> f (Right vf)
  Nothing   -> pure (Right vf)

-- | If the 'VirtualFile' has an embedded value, traverses to it.
-- TODO: Explain the relation of VirtualFiles and embedded values.
-- A VirtualFile might have no embedded value?
-- Is the embedded value meant to be written?
-- Is the embedded value to be considered an appendage of VirtualFiles
-- with no relation to the data that can be read or written to them?
-- If it is related, it comes after or before the data that can be read with the
-- serials? Perhaps these questions belong to SerializationMethod.
vfileEmbeddedValue :: Traversal' (VirtualFile a b) b
vfileEmbeddedValue = vfileSerials . serialReaders . serialReaderEmbeddedValue . traversed

-- | If the 'VirtualFile' has an embedded value and converters to and from a
-- type @c@, we traverse to a value of this type. The conversion can fail
vfileIntermediaryValue
  :: forall a c. (Typeable c)
  => Traversal' (Either String (VirtualFile a a)) c
vfileIntermediaryValue _ (Left s) = pure (Left s)
vfileIntermediaryValue f (Right vf) = case convertFns of
  Just (ToAtomicFn toA, FromAtomicFn fromA) ->
    let processVal v = case cast (toA v) of
          Nothing -> error $ "vfileIntermediaryValue: Impossible to cast the value to type "
            ++ show resTypeRep ++ ", however a conversion function was declared in the writers"
          Just i -> back <$> f i
        back i' = case cast i' of
          Nothing -> error $ "vfileIntermediaryValue: Impossible to cast back the value from type "
            ++ show resTypeRep ++ ", however a conversion function was declared in the readers"
          Just i'' -> fromA i''
    in getCompose $ vfileEmbeddedValue (Compose . processVal) vf
  Nothing -> pure (Right vf)
  where
    resTypeRep = typeOf (undefined :: c)
    serials = _vfileSerials vf
    convertFns = (,)
      <$> HM.lookup (resTypeRep,Nothing) (_serialWritersToAtomic (_serialWriters serials))
      <*> HM.lookup (resTypeRep,Nothing) (_serialReadersFromAtomic (_serialReaders serials))

-- | If the file has a defaut value and a way to convert it to/from aeson Value,
-- we traverse to it. Note that @b@ DOESN'T need to have To/FromJSON
-- instances. because the virtual file serials may contain a way to convert it
-- to and from a type that has these instances. So it isn't the same as calling
-- 'vfileEmbeddedValue' and converting the traversed value.
vfileAesonValue :: Traversal' (Either String (VirtualFile a a)) Value
vfileAesonValue = vfileIntermediaryValue


-- * Compatibility layer with the doc records of options used by the
-- command-line parser

-- | Contains any set of options that should be exposed via the CLI
data RecOfOptions field where
  RecOfOptions :: (Typeable rs, RecordUsableWithCLI rs) => Rec field rs -> RecOfOptions field

type DocRecOfOptions = RecOfOptions DocField

-- | If the file is bidirectional and has an embedded value that can be
-- converted to and from DocRecords (so that it can be read from config file AND
-- command-line), we traverse to it. Setting it changes the value embedded in
-- the file to reflect the new record of options. BEWARE not to change the
-- fields when setting the new doc record.
vfileRecOfOptions :: forall a. Traversal' (VirtualFile a a) DocRecOfOptions
vfileRecOfOptions f vf = case mbopts of
  Just (WriteToConfigFn convert, defVal, convertBack) ->
    rebuild convertBack <$> f (RecOfOptions $ convert defVal)
  _ -> pure vf
  where
    serials = _vfileSerials vf
    First mbopts = (,,)
      <$> _serialWriterToConfig (_serialWriters serials)
      <*> _serialReaderEmbeddedValue (_serialReaders serials)
      <*> _serialReaderFromConfig (_serialReaders serials)

    rebuild :: ReadFromConfigFn a -> DocRecOfOptions -> VirtualFile a a
    rebuild (ReadFromConfigFn convertBack) (RecOfOptions r) =
      let newVal = case cast r of
            Nothing -> error "vfileRecOfOptions: record fields aren't compatible"
            Just r' -> convertBack r'
      in vf & vfileEmbeddedValue .~ newVal

-- | Gives access to a version of the VirtualFile without type params. The
-- original path isn't settable.
vfileVoided :: Lens' (VirtualFile a b) (VirtualFile Void ())
vfileVoided f (VirtualFile p l m i d b s) =
  rebuild <$> f (VirtualFile p SingleLayerRead m i d Nothing mempty)
  where
    rebuild (VirtualFile _ _ m' i' d' _ _) =
      VirtualFile p l m' i' d' b s
