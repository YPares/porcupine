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

import           Control.Lens
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
import           Data.Typeable
import           Data.Void


-- | How to read an @a@ from some identified type @i@, which is meant to be a
-- general-purpose intermediate representation, like 'A.Value'.
data FromIntermediaryFn a =
  forall i. (Typeable i) => FromIntermediaryFn (i -> Either String a)
deriving instance Functor FromIntermediaryFn

instance Show (FromIntermediaryFn a) where
  show _ = "<FromIntermediaryFn>"

singletonFromIntermediaryFn
  :: forall i a. (Typeable i) => (i -> Either String a) -> HM.HashMap TypeRep (FromIntermediaryFn a)
singletonFromIntermediaryFn f = HM.singleton argTypeRep (FromIntermediaryFn f)
  where argTypeRep = typeOf (undefined :: i)

-- | How to read an @a@ from some file, in any 'LocationMonad'.
data ReadFromLoc a =
  ReadFromLoc { _readFromLocRepetitionKeys :: [LocVariable]
              , _readFromLocPerform        ::
                  (forall m. (LocationMonad m, LogThrow m)
                  => Loc -> m a)
              }
  deriving (Functor)

instance Show (ReadFromLoc a) where
  show _ = "<ReadFromLoc>"

makeLenses ''ReadFromLoc

-- | Constructs a 'ReadFromLoc' that will not expect any repetition.
simpleReadFromLoc
  :: (forall m. (LocationMonad m, MonadThrow m) => Loc -> m a)
  -> ReadFromLoc a
simpleReadFromLoc f = ReadFromLoc [] f

-- | A function to read @a@ from a 'DocRec'
data ReadFromConfigFn a = forall rs. (Typeable rs) => ReadFromConfigFn (DocRec rs -> a)
deriving instance Functor ReadFromConfigFn

instance Show (ReadFromConfigFn a) where
  show _ = "<ReadFromConfigFn>"

-- | A file extension
type FileExt = T.Text

-- | Here, "serial" is short for "serialization method". 'SerialReaders' is the
-- **covariant** part of 'SerialsFor'. It describes the different ways a serial
-- can be used to obtain data.
data SerialReaders a = SerialReaders
  { _serialReadersFromIntermediary :: HM.HashMap TypeRep (FromIntermediaryFn a)
       -- ^ How to read data from an intermediate type (like 'A.Value' or
       -- 'T.Text') that should be directly read from the pipeline's
       -- configuration
  , _serialReaderFromConfig        :: First (ReadFromConfigFn a)
    -- ^ How to read data from the CLI. It can be seen as a special kind of
    -- reader FromIntermediary.
  , _serialReaderEmbeddedValue     :: First a
      -- ^ Simply read from an embedded value. Depending on when this field is
      -- accessed, it can correspond to a default value or the value we read
      -- from the configuration.
  , _serialReadersFromInputFile    :: HM.HashMap FileExt (ReadFromLoc a)
       -- ^ How to read data from an external file or data storage.
  }
  deriving (Functor, Show)

makeLenses ''SerialReaders

instance Semigroup (SerialReaders a) where
  SerialReaders i c v f <> SerialReaders i' c' v' f' =
    SerialReaders (HM.unionWith const i i') (c<>c') (v<>v') (HM.unionWith const f f')
instance Monoid (SerialReaders a) where
  mempty = SerialReaders mempty mempty mempty mempty

-- | How to turn an @a@ into some identified type @i@, which is meant to a
-- general purpose intermediate representation, like 'A.Value' or even 'T.Text'.
data ToIntermediaryFn a =
  forall i. (Typeable i) => ToIntermediaryFn (a -> i)

instance Show (ToIntermediaryFn a) where
  show _ = "<ToIntermediaryFn>"

singletonToIntermediaryFn :: (Typeable i) => (a -> i) -> HM.HashMap TypeRep (ToIntermediaryFn a)
singletonToIntermediaryFn f = HM.singleton (typeOf $ f undefined) (ToIntermediaryFn f)

-- | How to write an @a@ to some file, in any 'LocationMonad'.
data WriteToLoc a = WriteToLoc
  { _writeToLocRepetitionKeys :: [LocVariable]
  , _writeToLocPerform ::
      (forall m. (LocationMonad m, LogThrow m)
      =>  a -> Loc -> m ())
  }

instance Show (WriteToLoc a) where
  show (WriteToLoc rks _) = "<WriteToLoc, rep keys: " ++ show rks ++ ">"

makeLenses ''WriteToLoc

-- | Constructs a 'WriteToLoc' that will not expect any repetition.
simpleWriteToLoc
  :: (forall m. (LocationMonad m, MonadThrow m) => a -> Loc -> m ())
  -> WriteToLoc a
simpleWriteToLoc f = WriteToLoc [] f

-- | The contravariant part of 'ReadFromConfigFn'. Permits to write default values
-- of the input config
data WriteToConfigFn a = forall rs. (Typeable rs, RecordUsableWithCLI rs)
                      => WriteToConfigFn (a -> DocRec rs)

instance Show (WriteToConfigFn a) where
  show _ = "<WriteToConfigFn>"

-- | The writing part of a serial. 'SerialWriters' describes the different ways
-- a serial can be used to serialize (write) data.
data SerialWriters a = SerialWriters
  { _serialWritersToIntermediary :: HM.HashMap TypeRep (ToIntermediaryFn a)
      -- ^ How to write the data to an intermediate type (like 'A.Value') that
      -- should be integrated to the stdout of the pipeline.
  , _serialWriterToConfig        :: First (WriteToConfigFn a)
  , _serialWritersToOutputFile   :: HM.HashMap FileExt (WriteToLoc a)
      -- ^ How to write the data to an external file or storage.
  }
  deriving (Show)

makeLenses ''SerialWriters

instance Semigroup (SerialWriters a) where
  SerialWriters i c f <> SerialWriters i' c' f' =
    SerialWriters (HM.unionWith const i i') (c<>c') (HM.unionWith const f f')
instance Monoid (SerialWriters a) where
  mempty = SerialWriters mempty mempty mempty

instance Contravariant SerialWriters where
  contramap f sw = SerialWriters
    { _serialWritersToIntermediary = fmap (\(ToIntermediaryFn f') -> ToIntermediaryFn $ f' . f)
                                     (_serialWritersToIntermediary sw)
    , _serialWriterToConfig = fmap (\(WriteToConfigFn f') -> WriteToConfigFn $ f' . f)
                              (_serialWriterToConfig sw)
    , _serialWritersToOutputFile = fmap (\(WriteToLoc rk f') ->
                                           WriteToLoc rk $ f' . f)
                                   (_serialWritersToOutputFile sw)
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
    { _serialWritersToIntermediary = singletonToIntermediaryFn A.toJSON
    , _serialWritersToOutputFile   = HM.singleton "json" $ simpleWriteToLoc write
    } where
    write x loc = Loc.writeLazyByte loc $ A.encode x

parseJSONEither :: (A.FromJSON t) => A.Value -> Either String t
parseJSONEither x = case A.fromJSON x of
  A.Success s -> Right s
  A.Error r   -> Left r
{-# INLINE parseJSONEither #-}

instance (FromJSON a) => DeserializesWith (JSONSerial a) a where
  getSerialReaders _ = mempty
    { _serialReadersFromIntermediary = singletonFromIntermediaryFn parseJSONEither
    , _serialReadersFromInputFile    = HM.singleton "json" $ simpleReadFromLoc readFn
    } where
    readFn loc = Loc.readLazyByte loc >>= withReadError >>= decodeWithLoc loc
    withReadError (Right x)  = return x
    withReadError (Left err) = throwM $ FileReadError err
    decodeWithLoc loc x = case A.eitherDecode x of
      Right y  -> return y
      Left msg -> throwM $ DecodingError loc $ T.pack msg


-- | The crudest SerializationMethod there is. Can read from text files or raw
-- input strings in the pipeline configuration file. Should be used only for
-- small files or input strings. The prefered file extension is the first of the
-- list.
newtype PlainTextSerial = PlainTextSerial { plainTextSerialFileExtensions :: [FileExt] }

instance SerializationMethod PlainTextSerial where
  getSerialDefaultExt (PlainTextSerial exts) = Just $ head exts

instance SerializesWith PlainTextSerial T.Text where
  getSerialWriters (PlainTextSerial exts) = mempty
    { _serialWritersToIntermediary =
        singletonToIntermediaryFn id <> singletonToIntermediaryFn toJSON
        -- A text can be written to a raw string or a String field in a JSON
        -- output
    , _serialWritersToOutputFile = HM.fromList $ map (,writeFn) exts
    } where
    writeFn = simpleWriteToLoc $ \x loc -> writeText loc x

instance DeserializesWith PlainTextSerial T.Text where
  getSerialReaders (PlainTextSerial exts) = mempty
    { _serialReadersFromIntermediary =
        singletonFromIntermediaryFn Right <> singletonFromIntermediaryFn parseJSONEither
    , _serialReadersFromInputFile = HM.fromList $ map (,simpleReadFromLoc readFromLoc) exts
    } where
    readFromLoc loc = do
      res <- readText loc
      case res of
        Left err -> throwM err
        Right r  -> return r

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
  , customPureDeserialRead       :: forall m. (LocationMonad m) => Loc -> m a
                                 -- ^ Reading function
  }
instance SerializationMethod (CustomPureDeserial a) where
  getSerialDefaultExt (CustomPureDeserial exts _) = Just $ head exts
instance DeserializesWith (CustomPureDeserial a) a where
  getSerialReaders (CustomPureDeserial exts f) = mempty
    { _serialReadersFromInputFile = HM.fromList $ map (,simpleReadFromLoc f) exts }


-- | Can serialize @a@ and deserialize @b@.
data SerialsFor a b = SerialsFor
  { _serialWriters    :: SerialWriters a
  , _serialReaders    :: SerialReaders b
  , _serialDefaultExt :: First FileExt }
  deriving (Show)

makeLenses ''SerialsFor

-- | Can serialize and deserialize @a@. Use 'dimap' to transform it
type BidirSerials a = SerialsFor a a

-- | Can only serialize @a@. Use 'lmap' to transform it.
type PureSerials a = SerialsFor a ()

-- | Can only deserialize @a@. Use 'rmap' to transform it.
type PureDeserials a = SerialsFor Void a

instance Profunctor SerialsFor where
  lmap f (SerialsFor sers desers ext) = SerialsFor (contramap f sers) desers ext
  rmap f (SerialsFor sers desers ext) = SerialsFor sers (fmap f desers) ext

instance Semigroup (SerialsFor a b) where
  SerialsFor s d ext <> SerialsFor s' d' ext' =
    SerialsFor (s<>s') (d<>d') (ext<>ext')
instance Monoid (SerialsFor a b) where
  mempty = SerialsFor mempty mempty mempty

-- | Packs together ways to serialize and deserialize some data @a@
someBidirSerial :: (SerializesWith s a, DeserializesWith s a) => s -> BidirSerials a
someBidirSerial s =
  SerialsFor (getSerialWriters s) (getSerialReaders s) (First $ getSerialDefaultExt s)

makeBidir :: PureSerials a -> PureDeserials a -> BidirSerials a
makeBidir (SerialsFor sers _ ext) (SerialsFor _ desers ext') =
  SerialsFor sers desers (ext<>ext')

-- | Packs together ways to serialize some data @a@
somePureSerial :: (SerializesWith s a) => s -> PureSerials a
somePureSerial s =
  SerialsFor (getSerialWriters s) mempty (First $ getSerialDefaultExt s)

-- | Packs together ways to deserialize and deserialize some data @a@
somePureDeserial :: (DeserializesWith s a) => s -> PureDeserials a
somePureDeserial s = SerialsFor mempty (getSerialReaders s) (First $ getSerialDefaultExt s)

eraseSerials :: SerialsFor a b -> PureDeserials b
eraseSerials (SerialsFor _ desers ext) = SerialsFor mempty desers ext

eraseDeserials :: SerialsFor a b -> PureSerials a
eraseDeserials (SerialsFor sers _ ext) = SerialsFor sers mempty ext


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
  -> (forall m. (LocationMonad m) => Loc -> m a)
  -> PureDeserials a
customPureDeserial exts f = somePureDeserial $ CustomPureDeserial exts f

-- | Traverses to the repetition keys stored in the access functions of a
-- 'SerialsFor'
serialsRepetitionKeys :: Traversal' (SerialsFor a b) [LocVariable]
serialsRepetitionKeys f (SerialsFor writers readers ext) =
  rebuild <$> (serialWritersToOutputFile . traversed . writeToLocRepetitionKeys) f writers
          <*> (serialReadersFromInputFile . traversed . readFromLocRepetitionKeys) f readers
  where
    rebuild w r = SerialsFor w r ext
