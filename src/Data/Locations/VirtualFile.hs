{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Locations.VirtualFile
  ( LocationTreePathItem
  , module Data.Locations.SerializationMethod
  , Profunctor(..)
  , VirtualFile_(..), VFMetadata(..)
  , VirtualFile, BidirVirtualFile, DataSource, DataSink
  , VirtualFileIntent(..), VirtualFileDescription(..)
  , DocRecOfOptions, RecOfOptions(..)
  , vfileStateData, vfileBidirProof, vfileSerials
  , vfileAsBidir
  , vfileEmbeddedValue, vfileIntermediaryValue, vfileAesonValue
  , isVFileUsedByDefault, vfilePath
  , dataSource, dataSink, bidirVirtualFile, ensureBidirFile
  , makeSink, makeSource
  , documentedFile, unusedByDefault
  , removeVFilePath
  , getVirtualFileDescription
  , vfileRecOfOptions
  ) where

import           Control.Lens
import           Data.Locations.LocationTree
import           Data.Locations.SerializationMethod
import Data.Aeson (Value, toJSON)
import Data.DocRecord
import Data.DocRecord.OptParse (RecordUsableWithCLI)
import           Data.Monoid                        (First (..))
import           Data.Profunctor                    (Profunctor (..))
import qualified Data.Text                          as T
import Data.Typeable
import           Data.Void
import Data.Type.Equality
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Locations.Mappings (HasDefaultMappingRule(..))


-- * The general 'VirtualFile_' type

-- | A virtual file in the location tree to which we can write @a@ and from
-- which we can read @b@.
data VirtualFile_ d a b = VirtualFile
  { _vfileStateData :: d
  , _vfileBidirProof :: First (a :~: b)
                    -- Temporary, necessary until we can do away with docrec
                    -- conversion in the writer part of SerialsFor
  , _vfileSerials    :: SerialsFor a b }

makeLenses ''VirtualFile_

instance (HasDefaultMappingRule d) => HasDefaultMappingRule (VirtualFile_ d a b) where
  isMappedByDefault = isMappedByDefault . _vfileStateData

instance Semigroup d => Semigroup (VirtualFile_ d a b) where
  VirtualFile d b s <> VirtualFile d' b' s' =
    VirtualFile (d<>d') (b<>b') (s<>s')
instance Monoid d => Monoid (VirtualFile_ d a b) where
  mempty = VirtualFile mempty mempty mempty

fn :: First a
fn = First Nothing

instance Profunctor (VirtualFile_ d) where
  dimap f g (VirtualFile d _ s) = VirtualFile d fn $ dimap f g s


-- * Obtaining a description of how the 'VirtualFile' should be used

-- | Describes how a virtual file is meant to be used
data VirtualFileIntent =
  VFForWriting | VFForReading | VFForRW | VFForCaching | VFForCLIOptions
  deriving (Show, Eq)

-- | Gives the purpose of the 'VirtualFile'. Used to document the pipeline and check
-- mappings to physical files.
data VirtualFileDescription = VirtualFileDescription
  { vfileDescIntent :: Maybe VirtualFileIntent
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
getVirtualFileDescription :: VirtualFile_ d a b -> VirtualFileDescription
getVirtualFileDescription (VirtualFile _ bidir (SerialsFor (SerialWriters toI toC toE)
                                                           (SerialReaders fromI fromC fromV fromE)
                                                           prefExt)) =
  VirtualFileDescription intent readableFromConfig writableInOutput exts
  where
    intent
      | First (Just _) <- fromC, First (Just _) <- fromV, First (Just _) <- toC = Just VFForCLIOptions
      | HM.null fromE && HM.null toE = Nothing
      | HM.null fromE = Just VFForWriting
      | HM.null toE = Just VFForReading
      | First (Just _) <- bidir = Just VFForCaching
      | otherwise = Just VFForRW
    otherExts = HS.fromList $ HM.keys toE <> HM.keys fromE
    exts = case prefExt of
             First (Just e) -> e:(HS.toList $ HS.delete e otherExts)
             _ -> HS.toList otherExts
    typeOfAesonVal = typeOf (undefined :: Value)
    readableFromConfig = typeOfAesonVal `HM.member` fromI
    writableInOutput = typeOfAesonVal `HM.member` toI


-- * The more specific 'VirtualFile' type (a 'VirtualFile_' with metadata)

data VFMetadata = VFMetadata
  { _vfileMD_UsedByDefault :: Bool
  , _vfileMD_Documentation :: First T.Text }

instance HasDefaultMappingRule VFMetadata where
  isMappedByDefault = _vfileMD_UsedByDefault

makeLenses ''VFMetadata

instance Semigroup VFMetadata where
  VFMetadata u d <> VFMetadata u' d' = VFMetadata (u && u') (d<>d')

-- | A 'VirtualFile', as declared by an application.
type VirtualFile = VirtualFile_ ([LocationTreePathItem], VFMetadata)

removeVFilePath :: VirtualFile a b -> VirtualFile_ VFMetadata a b
removeVFilePath vf = vf & vfileStateData %~ view _2

vfilePath :: VirtualFile a b -> [LocationTreePathItem]
vfilePath = view (vfileStateData . _1)

isVFileUsedByDefault :: VirtualFile a b -> Bool
isVFileUsedByDefault = view (vfileStateData . _2 . vfileMD_UsedByDefault)

-- | Indicates that the file should be mapped to 'null' by default
unusedByDefault :: VirtualFile a b -> VirtualFile a b
unusedByDefault = vfileStateData . _2 . vfileMD_UsedByDefault .~ False

-- | Gives a documentation to the 'VirtualFile'
documentedFile :: T.Text -> VirtualFile a b -> VirtualFile a b
documentedFile doc = vfileStateData . _2 . vfileMD_Documentation .~ First (Just doc)


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
virtualFile path refl sers =
  VirtualFile (path, VFMetadata True fn) (First refl) sers

-- | Creates a virtual file from its virtual path and ways to deserialize the
-- data.
dataSource :: [LocationTreePathItem] -> SerialsFor a b -> DataSource b
dataSource path = virtualFile path Nothing . eraseSerials

-- | Creates a virtual file from its virtual path and ways to serialize the
-- data.
dataSink :: [LocationTreePathItem] -> SerialsFor a b -> DataSink a
dataSink path = virtualFile path Nothing . eraseDeserials

-- | Like VirtualFile, except we will embed the proof that @a@ and @b@ are the same
bidirVirtualFile :: [LocationTreePathItem] -> BidirSerials a -> BidirVirtualFile a
bidirVirtualFile path sers = virtualFile path (Just Refl) sers

-- | If a file has been transformed via Profunctor methods (dimap, rmap, rmap),
-- even if it remained bidirectional this proof it is has been lost. Thus we
-- have to embed it again.
ensureBidirFile :: VirtualFile_ d a a -> VirtualFile_ d a a
ensureBidirFile vf = vf{_vfileBidirProof=First $ Just Refl}

makeSink :: VirtualFile a b -> DataSink a
makeSink vf = vf{_vfileSerials=eraseDeserials $ _vfileSerials vf
                ,_vfileBidirProof=fn}

makeSource :: VirtualFile a b -> DataSource b
makeSource vf = vf{_vfileSerials=eraseSerials $ _vfileSerials vf
                  ,_vfileBidirProof=fn}


-- * Traversals to the content of the VirtualFile, when it already embeds some
-- value

-- | If we have the internal proof that a VirtualFile is actually bidirectional,
-- we convert it.
vfileAsBidir :: Traversal' (VirtualFile_ d a b) (VirtualFile_ d a a)
vfileAsBidir f vf = case _vfileBidirProof vf of
  First (Just Refl) -> f vf
  First Nothing     -> pure vf

-- | If the 'VirtualFile' has an embedded value, traverses to it.
vfileEmbeddedValue :: Traversal' (VirtualFile_ d a b) b
vfileEmbeddedValue = vfileSerials . serialReaders . serialReaderEmbeddedValue . traversed

-- | If the 'VirtualFile' has an embedded value and converters to and from a
-- type @c@, we traverse to a value of this type.
vfileIntermediaryValue :: forall a c d. (Typeable c) => Traversal' (VirtualFile_ d a a) c
vfileIntermediaryValue f vf = case convertFns of
  Just (ToIntermediaryFn toI, FromIntermediaryFn fromI) ->
    let processVal v = case cast (toI v) of
          Nothing -> error $ "vfileIntermediaryValue: Impossible to cast the value to type "
            ++ show resTypeRep ++ ", however a conversion function was declared in the writers"
          Just i -> back <$> f i
        back i' = case cast i' of
          Nothing -> error $ "vfileIntermediaryValue: Impossible to cast back the value from type "
            ++ show resTypeRep ++ ", however a conversion function was declared in the readers"
          Just i'' -> case fromI i'' of
            Left s -> error $ "vfileIntermediaryValue: " ++ s
            Right x -> x
    in vfileEmbeddedValue processVal vf
  Nothing -> pure vf
  where
    resTypeRep = typeOf (undefined :: c)
    serials = _vfileSerials vf
    convertFns = (,)
      <$> HM.lookup resTypeRep (_serialWritersToIntermediary (_serialWriters serials))
      <*> HM.lookup resTypeRep (_serialReadersFromIntermediary (_serialReaders serials))

-- | If the file has a defaut value and a way to convert it to/from aeson Value,
-- we traverse to it. Note that @b@ DOESN'T need to have To/FromJSON
-- instances. because the virtual file serials may contain a way to convert it
-- to and from a type that has these instances. So it isn't the same as calling
-- 'vfileEmbeddedValue' and converting the traversed value.
vfileAesonValue :: Traversal' (VirtualFile_ d a a) Value
vfileAesonValue = vfileIntermediaryValue


-- * Compatibility layer with the doc records of options used by the
-- command-line parser

-- | Contains any set of options that should be exposed via the CLI
data RecOfOptions field where
  RecOfOptions :: (Typeable rs, RecordUsableWithCLI rs) => Rec field rs -> RecOfOptions field

type DocRecOfOptions = RecOfOptions DocField

-- | If the file is bidirectional and has a defaut value that can be converted
-- to and from docrecords (so that it can be read from config file AND
-- command-line), we traverse to it. Setting it changes the value embedded in
-- the file to reflect the new record of options. BEWARE not to change the
-- fields when setting the new doc record.
vfileRecOfOptions :: forall d a. Traversal' (VirtualFile_ d a a) DocRecOfOptions
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

    rebuild :: ReadFromConfigFn a -> DocRecOfOptions -> VirtualFile_ d a a
    rebuild (ReadFromConfigFn convertBack) (RecOfOptions r) =
      let newVal = case cast r of
            Nothing -> error "vfileRecOfOptions: record fields aren't compatible"
            Just r' -> convertBack r'
      in vf & vfileEmbeddedValue .~ newVal
