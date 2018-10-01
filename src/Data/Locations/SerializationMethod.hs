{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# OPTIONS_GHC -Wall #-}

module Data.Locations.SerializationMethod where

import           Control.Applicative
import           Control.Monad.Catch
import           Data.Aeson                   as A
import           Data.Binary
import           Data.Default
import           Data.Functor.Contravariant
import           Data.Hashable
import           Data.Locations.Loc           as Loc
import           Data.Locations.LocationMonad as Loc
import           Data.Monoid                  (First (..))
import qualified Data.HashMap.Strict          as HM
import           Data.Profunctor
import           Data.Representable
import qualified Data.Text                    as T
import           Data.Typeable
import           Data.Void
import           GHC.Generics
import qualified Options.Applicative          as O


-- | Some locs will allow several serialization methods to be used, but often we
-- will just LocDefault (JSON for local files and S3 objects). They have some
-- priority order.
--
-- Currently, this type is used mostly for defining file extensions, and should
-- be removed in the future. See the newest 'SerializationMethod' class that
-- handles the serialization/deserialization code per se.
data SerialMethod =
  LocDefault | JSON | CSV | Markdown | PDF | Unusable | NullMapping
  deriving (Eq, Ord, Show, Read, Generic, Hashable, ToJSON, Binary)

instance Default SerialMethod where
  def = LocDefault

instance Semigroup SerialMethod where
  a <> b = min a b
instance Monoid SerialMethod where
  mempty = LocDefault

instance Representable SerialMethod where
  toTextRepr LocDefault = ""
  toTextRepr Markdown   = "md"
  toTextRepr s          = T.toLower . T.pack . show $ s
  fromTextRepr x = case T.toLower x of
    ""     -> pure LocDefault
    "json" -> pure JSON
    "csv"  -> pure CSV
    "md"   -> pure Markdown
    "pdf"  -> pure PDF
    _      -> empty


data RetrievingError
  = FileReadError Loc.Error
  | DecodingError Loc.Loc T.Text

instance Exception RetrievingError

instance Show RetrievingError where
  show (FileReadError loc) = "Impossible to read file " <> show loc
  show (DecodingError loc msg) =
    "Error while decoding file " <> show loc <> ": " <> T.unpack msg

-- | How to read an @a@ from some identified type @i@, which is meant to be a
-- general-purpose intermediate representation, like 'A.Value'.
data FromIntermediaryFn a =
  forall i. (Typeable i) => FromIntermediaryFn (i -> Either String a)

singletonFromIntermediaryFn
  :: forall i a. (Typeable i) => (i -> Either String a) -> HM.HashMap TypeRep (FromIntermediaryFn a)
singletonFromIntermediaryFn f = HM.singleton argTypeRep (FromIntermediaryFn f)
  where argTypeRep = typeOf (undefined :: i)
    
-- | How to read an @a@ from some file, in any 'LocationMonad'.
newtype ReadFromLocFn a =
  ReadFromLocFn (forall m. (LocationMonad m, MonadThrow m) => Loc -> m a)

-- | Here, "serial" is short for "serialization method". 'SerialReaders'
-- describes the different ways a serial can be used to deserialize (read) data.
data SerialReaders a = SerialReaders
  { serialReaderFromNothing       :: First a
       -- ^ How to read data from 'null' (ie. when no input file is mapped)
  , serialReadersFromIntermediary :: HM.HashMap TypeRep (FromIntermediaryFn a)
       -- ^ How to read data from an intermediate type (like 'A.Value' or
       -- 'T.Text') that should be directly read from the pipeline's
       -- configuration
  , serialReaderFromCommandLine   :: First (O.Parser a)
       -- ^ How to read data from the CLI
  , serialReadersFromInputFile    :: HM.HashMap T.Text (ReadFromLocFn a)
       -- ^ How to read data from an external file or data storage.
  }

instance Semigroup (SerialReaders a) where
  SerialReaders n i c f <> SerialReaders n' i' c' f' =
    SerialReaders (n<>n') (HM.unionWith const i i') (c<>c') (HM.unionWith const f f')
instance Monoid (SerialReaders a) where
  mempty = SerialReaders mempty mempty mempty mempty

instance Functor SerialReaders where
  fmap f sr = SerialReaders
    { serialReaderFromNothing = f <$> serialReaderFromNothing sr
    , serialReadersFromIntermediary = fmap (\(FromIntermediaryFn f') -> FromIntermediaryFn $ fmap f . f')
                                      (serialReadersFromIntermediary sr)
    , serialReaderFromCommandLine = fmap f <$> serialReaderFromCommandLine sr
    , serialReadersFromInputFile = fmap (\(ReadFromLocFn f') -> ReadFromLocFn $ fmap f . f')
                                   (serialReadersFromInputFile sr)
    }

-- | How to turn an @a@ into some identified type @i@, which is meant to a
-- general purpose intermediate representation, like 'A.Value' or even 'T.Text'.
data ToIntermediaryFn a =
  forall i. (Typeable i) => ToIntermediaryFn (a -> i)

singletonToIntermediaryFn :: (Typeable i) => (a -> i) -> HM.HashMap TypeRep (ToIntermediaryFn a)
singletonToIntermediaryFn f = HM.singleton (typeOf $ f undefined) (ToIntermediaryFn f)

-- | How to write an @a@ to some file, in any 'LocationMonad'.
newtype WriteToLocFn a =
  WriteToLocFn (forall m. (LocationMonad m, MonadThrow m) => a -> Loc -> m ())

-- | The writing part of a serial. 'SerialWriters' describes the different ways
-- a serial can be used to serialize (write) data.
data SerialWriters a = SerialWriters
  { serialWritersToIntermediary :: HM.HashMap TypeRep (ToIntermediaryFn a)
      -- ^ How to write the data to an intermediate type (like 'A.Value') that
      -- should be integrated to the stdout of the pipeline.
  , serialWritersToOutputFile   :: HM.HashMap T.Text (WriteToLocFn a)
      -- ^ How to write the data to an external file or storage.
  }

instance Semigroup (SerialWriters a) where
  SerialWriters i f <> SerialWriters i' f' =
    SerialWriters (HM.unionWith const i i') (HM.unionWith const f f')
instance Monoid (SerialWriters a) where
  mempty = SerialWriters mempty mempty

instance Contravariant SerialWriters where
  contramap f sw = SerialWriters
    { serialWritersToIntermediary = fmap (\(ToIntermediaryFn f') -> ToIntermediaryFn $ f' . f)
                                    (serialWritersToIntermediary sw)
    , serialWritersToOutputFile = fmap (\(WriteToLocFn f') -> WriteToLocFn $ f' . f)
                                  (serialWritersToOutputFile sw)
    }

-- | Links a serialization method to a prefered file extension, if this is
-- relevant.
class SerializationMethod serial where
  -- | If not nothing, it should correspond to one of the keys in
  -- serialReadersFromInputFile or serialWritersToOutputFile.
  getSerialDefaultExt :: serial -> Maybe T.Text
  getSerialDefaultExt _ = Nothing

-- | Tells whether some type @a@ can be serialized by some _serial_ (serialization
-- method).
class (SerializationMethod serial) => SerializesWith serial a | serial -> a where
  getSerialWriters :: serial -> SerialWriters a

-- | Tells whether some type @a@ can be deserialized by some _serial_
-- (serialization method).
class (SerializationMethod serial) => DeserializesWith serial a | serial -> a where
  getSerialReaders :: serial -> SerialReaders a

-- | Only for serializing Void and deserializing ()
data VoidSerial = VoidSerial
instance SerializationMethod VoidSerial
instance SerializesWith VoidSerial Void where
  getSerialWriters _ = mempty
instance DeserializesWith VoidSerial () where
  getSerialReaders _ = mempty{serialReaderFromNothing = First $ Just ()}

-- | Has 'SerializesWith' & 'DeserializesWith' instances that permits to
-- store/load JSON files and 'A.Value's.
data JSONSerial a = JSONSerial

instance SerializationMethod (JSONSerial a) where
  getSerialDefaultExt _ = Just "json"

instance (ToJSON a) => SerializesWith (JSONSerial a) a where
  getSerialWriters _ = SerialWriters
    { serialWritersToIntermediary = singletonToIntermediaryFn A.encode
    , serialWritersToOutputFile   = HM.singleton "json" $ WriteToLocFn write
    } where
    write x loc = Loc.writeLazyByte loc $ A.encode x

instance (FromJSON a) => DeserializesWith (JSONSerial a) a where
  getSerialReaders _ = mempty
    { serialReadersFromIntermediary = singletonFromIntermediaryFn A.eitherDecode
    , serialReadersFromInputFile    = HM.singleton "json" $ ReadFromLocFn readFn
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
newtype PlainTextSerial = PlainTextSerial { plainTextSerialFileExtensions :: [T.Text] }

instance SerializationMethod PlainTextSerial where
  getSerialDefaultExt (PlainTextSerial exts) = Just $ head exts

instance SerializesWith PlainTextSerial T.Text where
  getSerialWriters (PlainTextSerial exts) = SerialWriters
    { serialWritersToIntermediary =
        singletonToIntermediaryFn id <> singletonToIntermediaryFn A.encode
        -- A text can be written to a raw string or a String field in a JSON
        -- output
    , serialWritersToOutputFile = HM.fromList $ map (,writeFn) exts
    } where
    writeFn = WriteToLocFn $ \x loc -> writeText loc x

instance DeserializesWith PlainTextSerial T.Text where
  getSerialReaders (PlainTextSerial exts) = mempty
    { serialReadersFromIntermediary =
        singletonFromIntermediaryFn Right <> singletonFromIntermediaryFn A.eitherDecode
    , serialReadersFromInputFile = HM.fromList $ map (,ReadFromLocFn readFromLoc) exts
    } where
    readFromLoc loc = do
      res <- readText loc
      case res of
        Left err -> throwM err
        Right r  -> return r

-- | A very simple deserial that deserializing nothing and just returs a default
-- value.
newtype DefaultValueDeserial a = DefaultValueDeserial a
instance SerializationMethod (DefaultValueDeserial a)
instance DeserializesWith (DefaultValueDeserial a) a where
  getSerialReaders (DefaultValueDeserial x) = mempty
    { serialReaderFromNothing = First $ Just x }

-- | A SerializationMethod that's meant to be used just for one datatype. Don't
-- abuse it.
data CustomPureSerial a =
  CustomPureSerial [T.Text] -- ^ Possible file extensions to write to
                   (forall m. (LocationMonad m) => a -> Loc -> m ()) -- ^ Writing function
instance SerializationMethod (CustomPureSerial a) where
  getSerialDefaultExt (CustomPureSerial exts _) = Just $ head exts
instance SerializesWith (CustomPureSerial a) a where
  getSerialWriters (CustomPureSerial exts f) = mempty
    { serialWritersToOutputFile = HM.fromList $ map (,WriteToLocFn f) exts
    }

-- | A DeserializationMethod that's meant to be used just for one
-- datatype. Don't abuse it.
data CustomPureDeserial a =
  CustomPureDeserial [T.Text] -- ^ Possible file extensions to read from
                     (forall m. (LocationMonad m) => Loc -> m a) -- ^ Reading function
instance SerializationMethod (CustomPureDeserial a) where
  getSerialDefaultExt (CustomPureDeserial exts _) = Just $ head exts
instance DeserializesWith (CustomPureDeserial a) a where
  getSerialReaders (CustomPureDeserial exts f) = mempty
    { serialReadersFromInputFile = HM.fromList $ map (,ReadFromLocFn f) exts
    }


-- | Can serialize @a@ and deserialize @b@.
data SerialsFor a b = SerialsFor
  { serialWriters :: SerialWriters a
  , serialReaders :: SerialReaders b
  , serialDefaultExt :: First T.Text }

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

voidSerial :: SerialWriters Void
voidSerial = getSerialWriters VoidSerial

voidDeserial :: SerialReaders ()
voidDeserial = getSerialReaders VoidSerial

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
  SerialsFor (getSerialWriters s) voidDeserial (First $ getSerialDefaultExt s)

-- | Packs together ways to deserialize and deserialize some data @a@
somePureDeserial :: (DeserializesWith s a) => s -> PureDeserials a
somePureDeserial s = SerialsFor voidSerial (getSerialReaders s) (First $ getSerialDefaultExt s)

eraseSerials :: SerialsFor a b -> PureDeserials b
eraseSerials (SerialsFor _ desers ext) = SerialsFor voidSerial desers ext

eraseDeserials :: SerialsFor a b -> PureSerials a
eraseDeserials (SerialsFor sers _ ext) = SerialsFor sers voidDeserial ext


-- | Builds a custom SerializationMethod (ie. which cannot be used for
-- deserialization) which is just meant to be used for one datatype.
customPureSerial
  :: [T.Text]   -- ^ The file extensions associated to this SerializationMethod
  -> (forall m. (LocationMonad m) => a -> Loc -> m ())
  -> PureSerials a
customPureSerial exts f =
  case mapM fromTextRepr exts of
    Nothing ->
      error $ "customPureSerial: some of " ++ show (map T.unpack exts) ++ " isn't associated with any SerialMethod"
    Just fts ->
      somePureSerial $ CustomPureSerial fts f

-- | Builds a custom SerializationMethod (ie. which cannot be used for
-- deserialization) which is just meant to be used for one datatype.
customPureDeserial
  :: [T.Text]   -- ^ The file extensions associated to this SerializationMethod
  -> (forall m. (LocationMonad m) => Loc -> m a)
  -> PureDeserials a
customPureDeserial exts f =
  case mapM fromTextRepr exts of
    Nothing ->
      error $ "customPureDeserial: some of " ++ show (map T.unpack exts) ++ " isn't associated with any SerialMethod"
    Just fts ->
      somePureDeserial $ CustomPureDeserial fts f

-- | To be used when the file is mapped to null. Won't read anything, will just
-- return the default value.
defaultValueDeserial :: a -> PureDeserials a
defaultValueDeserial = somePureDeserial . DefaultValueDeserial


-- * Functions for compatiblity with part of the API still using 'SerialMethod'.

indexPureSerialsByFileType :: SerialsFor a b -> HM.HashMap SerialMethod (WriteToLocFn a)
indexPureSerialsByFileType (SerialsFor sers _ _) =
  HM.fromList $ map (\(k,v) -> (associatedFileType k,v)) $ HM.toList $
  serialWritersToOutputFile sers

indexPureDeserialsByFileType :: SerialsFor a b -> HM.HashMap SerialMethod (ReadFromLocFn b)
indexPureDeserialsByFileType (SerialsFor _ desers _) =
  HM.fromList $ map (\(k,v) -> (associatedFileType k,v)) $ HM.toList $
  serialReadersFromInputFile desers

firstPureSerialFileType :: SerialsFor a b -> SerialMethod
firstPureSerialFileType (SerialsFor _ _ (First (Just ext))) = associatedFileType ext

firstPureDeserialFileType :: SerialsFor a b -> SerialMethod
firstPureDeserialFileType (SerialsFor _ _ (First (Just ext))) = associatedFileType ext

associatedFileType :: T.Text -> SerialMethod
associatedFileType x = case fromTextRepr x of
  Nothing -> error $ "No SerialMethod associated to: " ++ T.unpack x
  Just r  -> r
