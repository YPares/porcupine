{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# OPTIONS_GHC -Wall #-}

module Data.Locations.SerializationMethod where

import           Control.Lens                 hiding ((:>))
import           Data.Aeson                   as A
import           Data.DocRecord
import           Data.DocRecord.OptParse      (RecordUsableWithCLI)
import qualified Data.HashMap.Strict          as HM
import           Data.Locations.Loc           as Loc
import           Data.Locations.LocationMonad as Loc
import           Data.Locations.LocVariable
import           Data.Locations.LogAndErrors
import           Data.Monoid                  (First (..))
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import           Data.Typeable
import           Data.Void
import           Streaming
import qualified Streaming.Prelude as S
import qualified Data.ByteString.Streaming as BSS


-- | A file extension
type FileExt = T.Text

-- | How to read an @a@ from some identified type @i@, which is meant to be a
-- general-purpose intermediate representation, like 'A.Value'.
data FromAtomicFn a =
  forall i. (Typeable i) => FromAtomicFn (i -> Either String a)
deriving instance Functor FromAtomicFn

instance Show (FromAtomicFn a) where
  show _ = "<FromAtomicFn>"

singletonFromAtomicFn
  :: forall i a. (Typeable i)
  => Maybe FileExt
  -> (i -> Either String a)
  -> HM.HashMap (TypeRep,Maybe FileExt) (FromAtomicFn a)
singletonFromAtomicFn ext f = HM.singleton (argTypeRep,ext) (FromAtomicFn f)
  where argTypeRep = typeOf (undefined :: i)

-- | How to read an @a@ from some @Stream (Of i) m r@
data FromStreamFn a =
  forall i. (Typeable i)
  => FromStreamFn (forall m r. (LogMask m)
                   => Stream (Of i) m r -> m (Of a r))

instance Functor FromStreamFn where
  fmap f (FromStreamFn g) = FromStreamFn $ \s -> do
    (a :> r) <- g s
    return $ f a :> r

instance Show (FromStreamFn a) where
  show _ = "<FromStreamFn>"

singletonFromStreamFn
  :: forall i a. (Typeable i)
  => Maybe FileExt
  -> (forall m r. (LogMask m) => Stream (Of i) m r -> m (Of a r))
  -> HM.HashMap (TypeRep,Maybe FileExt) (FromStreamFn a)
singletonFromStreamFn ext f = HM.singleton (argTypeRep,ext) (FromStreamFn f)
  where argTypeRep = typeOf (undefined :: i)

-- -- | How to read an @a@ from some file, in any 'LocationMonad'.
-- data ReadFromLoc a =
--   ReadFromLoc { _readFromLocRepetitionKeys :: [LocVariable]
--               , _readFromLocPerform        ::
--                   (forall m. (LocationMonad m, LogThrow m)
--                   => Loc -> m a)
--               }
--   deriving (Functor)

-- | A function to read @a@ from a 'DocRec'
data ReadFromConfigFn a = forall rs. (Typeable rs) => ReadFromConfigFn (DocRec rs -> a)
deriving instance Functor ReadFromConfigFn

instance Show (ReadFromConfigFn a) where
  show _ = "<ReadFromConfigFn>"

-- | Here, "serial" is short for "serialization method". 'SerialReaders' is the
-- **covariant** part of 'SerialsFor'. It describes the different ways a serial
-- can be used to obtain data.
data SerialReaders a = SerialReaders
  { _serialReadersFromAtomic ::
        HM.HashMap (TypeRep,Maybe FileExt) (FromAtomicFn a)
       -- ^ How to read data from an intermediate type (like 'A.Value' or
       -- 'T.Text') that should be directly read from the pipeline's
       -- configuration
  , _serialReadersFromStream       ::
        HM.HashMap (TypeRep,Maybe FileExt) (FromStreamFn a)
       -- ^ How to read data from an external file or data storage.
  , _serialReaderFromConfig        :: First (ReadFromConfigFn a)
       -- ^ How to read data from the CLI. It can be seen as a special kind of
       -- reader FromAtomic.
  , _serialReaderEmbeddedValue     :: First a
      -- ^ Simply read from an embedded value. Depending on when this field is
      -- accessed, it can correspond to a default value or the value we read
      -- from the configuration.
  }
  deriving (Functor, Show)

makeLenses ''SerialReaders

instance Semigroup (SerialReaders a) where
  SerialReaders a s c v <> SerialReaders a' s' c' v' =
    SerialReaders (HM.unionWith const a a') (HM.unionWith const s s') (c<>c') (v<>v')
instance Monoid (SerialReaders a) where
  mempty = SerialReaders mempty mempty mempty mempty

-- | How to turn an @a@ into some identified type @i@, which is meant to a
-- general purpose intermediate representation, like 'A.Value' or even 'T.Text'.
data ToAtomicFn a =
  forall i. (Typeable i) => ToAtomicFn (a -> i)

instance Show (ToAtomicFn a) where
  show _ = "<ToAtomicFn>"

singletonToAtomicFn :: (Typeable i)
                    => Maybe FileExt
                    -> (a -> i)
                    -> HM.HashMap (TypeRep,Maybe FileExt) (ToAtomicFn a)
singletonToAtomicFn ext f = HM.singleton (typeOf $ f undefined,ext) (ToAtomicFn f)

-- | How to turn an @a@ into some @Stream (Of i) m ()@
data ToStreamFn a =
  forall i. (Typeable i)
  => ToStreamFn (forall m. (LogMask m)
                 => a -> Stream (Of i) m ())

instance Show (ToStreamFn a) where
  show _ = "<ToStreamFn>"

singletonToStreamFn
  :: forall i a. (Typeable i)
  => Maybe FileExt
  -> (forall m. (LogMask m) => a -> Stream (Of i) m ())
  -> HM.HashMap (TypeRep,Maybe FileExt) (ToStreamFn a)
singletonToStreamFn ext f = HM.singleton (argTypeRep,ext) (ToStreamFn f)
  where argTypeRep = typeOf (undefined :: i)

-- -- | How to write an @a@ to some file, in any 'LocationMonad'.
-- data WriteToLoc a = WriteToLoc
--   { _writeToLocRepetitionKeys :: [LocVariable]
--   , _writeToLocPerform ::
--       (forall m. (LocationMonad m, LogThrow m)
--       =>  a -> Loc -> m ())
--   }

-- | The contravariant part of 'ReadFromConfigFn'. Permits to write default values
-- of the input config
data WriteToConfigFn a = forall rs. (Typeable rs, RecordUsableWithCLI rs)
                      => WriteToConfigFn (a -> DocRec rs)

instance Show (WriteToConfigFn a) where
  show _ = "<WriteToConfigFn>"

-- | The writing part of a serial. 'SerialWriters' describes the different ways
-- a serial can be used to serialize (write) data.
data SerialWriters a = SerialWriters
  { _serialWritersToAtomic :: HM.HashMap (TypeRep,Maybe FileExt) (ToAtomicFn a)
      -- ^ How to write the data to an intermediate type (like 'A.Value') that
      -- should be integrated to the stdout of the pipeline.
  , _serialWritersToStream :: HM.HashMap (TypeRep,Maybe FileExt) (ToStreamFn a)
      -- ^ How to write the data to an external file or storage.
  , _serialWriterToConfig  :: First (WriteToConfigFn a)
  }
  deriving (Show)

makeLenses ''SerialWriters

instance Semigroup (SerialWriters a) where
  SerialWriters a s c <> SerialWriters a' s' c' =
    SerialWriters (HM.unionWith const a a') (HM.unionWith const s s') (c<>c')
instance Monoid (SerialWriters a) where
  mempty = SerialWriters mempty mempty mempty

instance Contravariant SerialWriters where
  contramap f sw = SerialWriters
    { _serialWritersToAtomic = fmap (\(ToAtomicFn f') -> ToAtomicFn $ f' . f)
                               (_serialWritersToAtomic sw)
    , _serialWritersToStream = fmap (\(ToStreamFn f') -> ToStreamFn $ f' . f)
                               (_serialWritersToStream sw)
    , _serialWriterToConfig = fmap (\(WriteToConfigFn f') -> WriteToConfigFn $ f' . f)
                              (_serialWriterToConfig sw)
    }

-- | Links a serialization method to a prefered file extension, if this is
-- relevant.
class SerializationMethod serial where
  -- | If not nothing, it should correspond to one of the keys in
  -- serialReadersFromInputFile or serialWritersToOutputFile.
  getSerialDefaultExt :: serial -> Maybe FileExt
  getSerialDefaultExt _ = Nothing

-- | Tells whether some type @a@ can be serialized by some _serial_ (serialization
-- method).
class (SerializationMethod serial) => SerializesWith serial a | serial -> a where
  getSerialWriters :: serial -> SerialWriters a

-- | Tells whether some type @a@ can be deserialized by some _serial_
-- (serialization method).
class (SerializationMethod serial) => DeserializesWith serial a | serial -> a where
  getSerialReaders :: serial -> SerialReaders a

-- | Has 'SerializesWith' & 'DeserializesWith' instances that permits to
-- store/load JSON files and 'A.Value's.
data JSONSerial a = JSONSerial

instance SerializationMethod (JSONSerial a) where
  getSerialDefaultExt _ = Just "json"

instance (ToJSON a) => SerializesWith (JSONSerial a) a where
  getSerialWriters _ = mempty
    { _serialWritersToAtomic = singletonToAtomicFn A.toJSON
    , _serialWritersToStream = singletonToAtomicFn A.encode x
    -- , _serialWritersToOutputFile = HM.singleton "json" $ simpleWriteToLoc write
    } where
    write x loc = Loc.writeLazyByte loc $ A.encode x

parseJSONEither :: (A.FromJSON t) => A.Value -> Either String t
parseJSONEither x = case A.fromJSON x of
  A.Success s -> Right s
  A.Error r   -> Left r
{-# INLINE parseJSONEither #-}

instance (FromJSON a) => DeserializesWith (JSONSerial a) a where
  getSerialReaders _ = mempty
    { _serialReadersFromAtomic =
        singletonFromAtomicFn Nothing parseJSONEither
    , _serialReadersFromStream =
        -- Decoding from a stream of bytestrings _only if it originates from a
        -- json source_
        singletonFromStreamFn (Just "json") $ \strm -> do
          (bs :> r) <- BSS.toLazy $ BSS.fromChunks strm
          (:> r) <$> decode_ bs
    -- , _serialReadersFromInputFile    = HM.singleton "json" $ simpleReadFromLoc readFn
    } where
    --readFn loc = Loc.readLazyByte loc >>= decodeWithLoc loc
    decode_ x = case A.eitherDecode x of
      Right y  -> return y
      Left msg -> throwString msg


-- | The crudest SerializationMethod there is. Can read from text files or raw
-- input strings in the pipeline configuration file. Should be used only for
-- small files or input strings. The prefered file extension is the first of the
-- list.
newtype PlainTextSerial = PlainTextSerial { plainTextSerialFileExtensions :: [FileExt] }

instance SerializationMethod PlainTextSerial where
  getSerialDefaultExt (PlainTextSerial exts) = Just $ head exts

instance SerializesWith PlainTextSerial T.Text where
  getSerialWriters (PlainTextSerial exts) = mempty
    { _serialWritersToAtomic =
        singletonToAtomicFn id <> singletonToAtomicFn toJSON
        -- A text can be written to a raw string or a String field in a JSON
        -- output
    , _serialWritersToOutputFile = HM.fromList $ map (,writeFn) exts
    } where
    writeFn = simpleWriteToLoc $ \x loc -> writeText loc x

instance DeserializesWith PlainTextSerial T.Text where
  getSerialReaders (PlainTextSerial exts) = mempty
    { _serialReadersFromAtomic =
        singletonFromAtomicFn Nothing Right
        <> singletonFromAtomicFn Nothing parseJSONEither
        <> singletonFromAtomicFn Nothing (Right . TE.decodeUtf8)
    , _serialReadersFromStream =
        mconcat $ map makeForExt exts
    -- , _serialReadersFromInputFile =
    --     HM.fromList $ map (,simpleReadFromLoc readText) exts
    } where
    makeForExt ext =
      singletonFromStreamFn (Just ext) S.mconcat
      <>
      singletonFromStreamFn (Just ext)
        (fmap (S.mapOf TE.decodeUtf8) . S.mconcat)

-- | A serialization method used for options which can have a default value,
-- that can be exposed through the configuration.
data DocRecSerial a = forall rs. (Typeable rs, RecordUsableWithCLI rs)
                   => DocRecSerial a (a -> DocRec rs) (DocRec rs -> a)
instance SerializationMethod (DocRecSerial a)
instance SerializesWith (DocRecSerial a) a where
  getSerialWriters (DocRecSerial _ f _) = mempty
    { _serialWriterToConfig = First $ Just $ WriteToConfigFn f }
instance DeserializesWith (DocRecSerial a) a where
  getSerialReaders (DocRecSerial d _ f) = mempty
    { _serialReaderEmbeddedValue = First $ Just d
    , _serialReaderFromConfig = First $ Just $ ReadFromConfigFn f }

-- -- | A very simple deserial that deserializing nothing and just returns a default
-- -- value.
-- newtype DefaultValueDeserial a = DefaultValueDeserial a
-- instance SerializationMethod (DefaultValueDeserial a)
-- instance DeserializesWith (DefaultValueDeserial a) a where
--   getSerialReaders (DefaultValueDeserial x) = mempty
--     { serialReaderFromNothing = First $ Just x }

-- | A SerializationMethod that's meant to be used just for one datatype. Don't
-- abuse it.
data CustomPureSerial a = CustomPureSerial
  { customPureSerialExtensions :: [FileExt]
                               -- ^ Possible file extensions to write to
  , customPureSerialWrite      :: forall m. (LocationMonad m) => a -> Loc -> m ()
                               -- ^ Writing function
  }
instance SerializationMethod (CustomPureSerial a) where
  getSerialDefaultExt (CustomPureSerial exts _) = Just $ head exts
instance SerializesWith (CustomPureSerial a) a where
  getSerialWriters (CustomPureSerial exts f) = mempty
    { _serialWritersToOutputFile = HM.fromList $ map (,simpleWriteToLoc f) exts
    }

-- | A DeserializationMethod that's meant to be used just for one
-- datatype. Don't abuse it.
data CustomPureDeserial a = CustomPureDeserial
  { customPureDeserialExtensions :: [FileExt]
                                 -- ^ Possible file extensions to read from
  , customPureDeserialRead       ::
      forall m r. (MonadMask m) => BSS.ByteString m r -> m (Of a r)
                                 -- ^ Reading function
  }
instance SerializationMethod (CustomPureDeserial a) where
  getSerialDefaultExt (CustomPureDeserial exts _) = Just $ head exts
instance DeserializesWith (CustomPureDeserial a) a where
  getSerialReaders (CustomPureDeserial exts f) = mempty
    { _serialReadersFromStream =
        mconcat $ map makeForExt exts }
    where
      makeForExt ext = singletonFromStreamFn (Just ext) $
                       f . BSS.fromChunks

-- | Can serialize @a@ and deserialize @b@.
data SerialsFor a b = SerialsFor
  { _serialWriters    :: SerialWriters a
  , _serialReaders    :: SerialReaders b
  , _serialDefaultExt :: First FileExt
  , _serialRepetitionKeys :: [LocVariable] }
  deriving (Show)

makeLenses ''SerialsFor

-- | Can serialize and deserialize @a@. Use 'dimap' to transform it
type BidirSerials a = SerialsFor a a

-- | Can only serialize @a@. Use 'lmap' to transform it.
type PureSerials a = SerialsFor a ()

-- | Can only deserialize @a@. Use 'rmap' to transform it.
type PureDeserials a = SerialsFor Void a

instance Profunctor SerialsFor where
  lmap f (SerialsFor sers desers ext rk) = SerialsFor (contramap f sers) desers ext rk
  rmap f (SerialsFor sers desers ext rk) = SerialsFor sers (fmap f desers) ext rk

instance Semigroup (SerialsFor a b) where
  SerialsFor s d ext rk <> SerialsFor s' d' ext' _ =
    SerialsFor (s<>s') (d<>d') (ext<>ext') rk
instance Monoid (SerialsFor a b) where
  mempty = SerialsFor mempty mempty mempty []

-- | Packs together ways to serialize and deserialize some data @a@
someBidirSerial :: (SerializesWith s a, DeserializesWith s a) => s -> BidirSerials a
someBidirSerial s =
  SerialsFor (getSerialWriters s) (getSerialReaders s) (First $ getSerialDefaultExt s) []

makeBidir :: PureSerials a -> PureDeserials a -> BidirSerials a
makeBidir (SerialsFor sers _ ext rk) (SerialsFor _ desers ext' _) =
  SerialsFor sers desers (ext<>ext') rk

-- | Packs together ways to serialize some data @a@
somePureSerial :: (SerializesWith s a) => s -> PureSerials a
somePureSerial s =
  SerialsFor (getSerialWriters s) mempty (First $ getSerialDefaultExt s) []

-- | Packs together ways to deserialize and deserialize some data @a@
somePureDeserial :: (DeserializesWith s a) => s -> PureDeserials a
somePureDeserial s =
  SerialsFor mempty (getSerialReaders s) (First $ getSerialDefaultExt s) []

eraseSerials :: SerialsFor a b -> PureDeserials b
eraseSerials (SerialsFor _ desers ext rk) = SerialsFor mempty desers ext rk

eraseDeserials :: SerialsFor a b -> PureSerials a
eraseDeserials (SerialsFor sers _ ext rk) = SerialsFor sers mempty ext rk


-- | Builds a custom SerializationMethod (ie. which cannot be used for
-- deserialization) which is just meant to be used for one datatype.
customPureSerial
  :: [FileExt]   -- ^ The file extensions associated to this SerializationMethod
  -> (forall m. (LocationMonad m) => a -> Loc -> m ())
  -> PureSerials a
customPureSerial exts f = somePureSerial $ CustomPureSerial exts f

-- | Builds a custom SerializationMethod (ie. which cannot be used for
-- deserialization) which is just meant to be used for one datatype.
customPureDeserial
  :: [FileExt]   -- ^ The file extensions associated to this SerializationMethod
  -> (forall m r. (MonadMask m) => BSS.ByteString m r -> m (Of a r))
  -> PureDeserials a
customPureDeserial exts f = somePureDeserial $ CustomPureDeserial exts f

-- -- | Traverses to the repetition keys stored in the access functions of a
-- -- 'SerialsFor'
-- serialsRepetitionKeys :: Traversal' (SerialsFor a b) [LocVariable]
-- serialsRepetitionKeys f (SerialsFor writers readers ext rk) =
--   rebuild <$> (serialWritersToOutputFile . traversed . writeToLocRepetitionKeys) f writers
--           <*> (serialReadersFromInputFile . traversed . readFromLocRepetitionKeys) f readers
--   where
--     rebuild w r = SerialsFor w r ext rk
