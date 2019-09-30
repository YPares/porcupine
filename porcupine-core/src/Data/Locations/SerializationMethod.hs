{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeOperators             #-}
{-# OPTIONS_GHC -Wall #-}

module Data.Locations.SerializationMethod where

import           Control.Lens                hiding ((:>))
import           Data.Aeson                  as A
-- import qualified Data.Attoparsec.Lazy        as AttoL
import qualified Data.Binary.Builder         as BinBuilder
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Streaming   as BSS
import           Data.Char                   (ord)
import qualified Data.Csv                    as Csv
import qualified Data.Csv.Builder            as CsvBuilder
-- import qualified Data.Csv.Parser             as CsvParser
import           Codec.Compression.Zlib      as Zlib
import           Data.DocRecord
import           Data.DocRecord.OptParse     (RecordUsableWithCLI)
import qualified Data.HashMap.Strict         as HM
import           Data.Locations.LocVariable
import           Data.Locations.LogAndErrors
import           Data.Maybe
import           Data.Monoid                 (First (..))
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE
import qualified Data.Text.Lazy              as LT
import qualified Data.Text.Lazy.Encoding     as LTE
import           Data.Typeable
import qualified Data.Vector                 as V
import           Data.Void
import qualified Data.Yaml                   as Y
import           Katip
import           GHC.Generics
import           Streaming
import qualified Streaming.Prelude           as S
import qualified Streaming.Zip               as SZip


-- | A file extension
type FileExt = T.Text

type FromAtomicFn' i a = i -> Either String a

-- | How to read an @a@ from some identified type @i@, which is meant to be a
-- general-purpose intermediate representation, like 'A.Value'.
data FromAtomicFn a =
  forall i. (Typeable i) => FromAtomicFn (FromAtomicFn' i a)
deriving instance Functor FromAtomicFn

instance Show (FromAtomicFn a) where
  show _ = "<FromAtomicFn>"

fromAtomicFn
  :: forall i a. (Typeable i)
  => [Maybe FileExt]
  -> FromAtomicFn' i a
  -> HM.HashMap (TypeRep,Maybe FileExt) (FromAtomicFn a)
fromAtomicFn exts f = HM.fromList $ map (\ext -> ((argTypeRep,ext), FromAtomicFn f)) exts
  where
    argTypeRep = typeOf (undefined :: i)

allFromAtomicFnsWithType :: forall i ext a. (Typeable i)
                         => HM.HashMap (TypeRep,Maybe ext) (FromAtomicFn a)
                         -> [(ext, FromAtomicFn' i a)]
allFromAtomicFnsWithType = mapMaybe fltr . HM.toList
  where
    wanted = typeOf (undefined :: i)
    fltr ((_,Nothing),_) = Nothing
    fltr ((tr,Just ext), FromAtomicFn (f :: FromAtomicFn' i' a))
      | tr == wanted = case eqT :: Maybe (i:~:i') of
          Just Refl -> Just (ext, f)
          Nothing   -> error $ "allFromAtomicFnsWithType: some function doesn't deal with type "
                        ++ show wanted ++ " when it should"
      | otherwise = Nothing


type FromStreamFn' i a =
  forall m. (LogMask m) => Stream (Of i) m () -> m a

-- | How to read an @a@ from some @Stream (Of i) m r@
data FromStreamFn a =
  forall i. (Typeable i) => FromStreamFn (FromStreamFn' i a)

instance Functor FromStreamFn where
  fmap f (FromStreamFn g) = FromStreamFn $ \s -> do
    f <$> g s

instance Show (FromStreamFn a) where
  show _ = "<FromStreamFn>"

fromStreamFn
  :: forall i a. (Typeable i)
  => [Maybe FileExt]
  -> FromStreamFn' i a
  -> HM.HashMap (TypeRep,Maybe FileExt) (FromStreamFn a)
fromStreamFn exts f = HM.fromList $ map (\ext -> ((argTypeRep,ext), FromStreamFn f)) exts
  where
    argTypeRep = typeOf (undefined :: i)

newtype FromStreamFn'' i a = FromStreamFn'' (FromStreamFn' i a)

allFromStreamFnsWithType :: forall i ext a. (Typeable i)
                         => HM.HashMap (TypeRep,Maybe ext) (FromStreamFn a)
                         -> [(ext, FromStreamFn'' i a)]
allFromStreamFnsWithType = mapMaybe fltr . HM.toList
  where
    wanted = typeOf (undefined :: i)
    fltr ((_,Nothing),_) = Nothing
    fltr ((tr,Just ext), FromStreamFn (f :: FromStreamFn' i' a))
      | tr == wanted = case eqT :: Maybe (i:~:i') of
          Just Refl -> Just (ext, FromStreamFn'' f)
          Nothing   -> error $ "allFromStreamFnsWithType: some function doesn't deal with type "
                        ++ show wanted ++ " when it should"
      | otherwise = Nothing

-- | A function to read @a@ from a 'DocRec'
data ReadFromConfigFn a = forall rs. (Typeable rs) => ReadFromConfigFn (DocRec rs -> a)
deriving instance Functor ReadFromConfigFn

instance Show (ReadFromConfigFn a) where
  show _ = "<ReadFromConfigFn>"

-- | Here, "serial" is short for "serialization method". 'SerialReaders' is the
-- **covariant** part of 'SerialsFor'. It describes the different ways a serial
-- can be used to obtain data.
data SerialReaders a = SerialReaders
  { -- TODO: Establish whether we should remove readersFromAtomic? It is often
    -- equivalent to reading from a stream of just one element, and therefore
    -- mostly duplicates code.
    _serialReadersFromAtomic ::
        HM.HashMap (TypeRep,Maybe FileExt) (FromAtomicFn a)
       -- ^ How to read data from an intermediate type (like 'A.Value' or
       -- 'T.Text'). As much as possible these intermediate atomic
       -- representations should be **strict**.
  , _serialReadersFromStream ::
        HM.HashMap (TypeRep,Maybe FileExt) (FromStreamFn a)
       -- ^ How to read data from a stream of intermediate data types (like
       -- strict ByteStrings). Each one of them being strict as much as
       -- possible.
  }
  deriving (Functor, Show)

makeLenses ''SerialReaders

instance Semigroup (SerialReaders a) where
  SerialReaders a s <> SerialReaders a' s' =
    SerialReaders (HM.unionWith const a a') (HM.unionWith const s s')
instance Monoid (SerialReaders a) where
  mempty = SerialReaders mempty mempty

-- | How to turn an @a@ into some identified type @i@, which is meant to a
-- general purpose intermediate representation, like 'A.Value' or even 'T.Text'.
data ToAtomicFn a =
  forall i. (Typeable i) => ToAtomicFn (a -> i)

instance Show (ToAtomicFn a) where
  show _ = "<ToAtomicFn>"

toAtomicFn :: forall i a. (Typeable i)
           => [Maybe FileExt]
           -> (a -> i)
           -> HM.HashMap (TypeRep,Maybe FileExt) (ToAtomicFn a)
toAtomicFn exts f = HM.fromList $ map (\ext -> ((argTypeRep,ext), ToAtomicFn f)) exts
  where
    argTypeRep = typeOf (undefined :: i)

allToAtomicFnsWithType :: forall i ext a. (Typeable i)
                         => HM.HashMap (TypeRep,Maybe ext) (ToAtomicFn a)
                         -> [(ext, a -> i)]
allToAtomicFnsWithType = mapMaybe fltr . HM.toList
  where
    wanted = typeOf (undefined :: i)
    fltr ((_,Nothing),_) = Nothing
    fltr ((tr,Just ext), ToAtomicFn (f :: a -> i'))
      | tr == wanted = case eqT :: Maybe (i:~:i') of
          Just Refl -> Just (ext, f)
          Nothing   -> error $ "allToAtomicFnsWithType: some function doesn't deal with type "
                        ++ show wanted ++ " when it should"
      | otherwise = Nothing

-- -- | How to turn an @a@ into some @Stream (Of i) m ()@
-- data ToStreamFn a =
--   forall i. (Typeable i)
--   => ToStreamFn (forall m. (LogMask m)
--                  => a -> Stream (Of i) m ())

-- instance Show (ToStreamFn a) where
--   show _ = "<ToStreamFn>"

-- singletonToStreamFn
--   :: forall i a. (Typeable i)
--   => Maybe FileExt
--   -> (forall m. (LogMask m) => a -> Stream (Of i) m ())
--   -> HM.HashMap (TypeRep,Maybe FileExt) (ToStreamFn a)
-- singletonToStreamFn ext f = HM.singleton (argTypeRep,ext) (ToStreamFn f)
--   where argTypeRep = typeOf (undefined :: i)

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
      -- ^ How to write the data to an intermediate type (like 'A.Value'). As
      -- much as possible this intermediate type should be **lazy**.

  -- , _serialWritersToStream :: HM.HashMap (TypeRep,Maybe FileExt) (ToStreamFn a)
  --     -- ^ How to write the data to an external file or storage.
  }
  deriving (Show)

makeLenses ''SerialWriters

instance Semigroup (SerialWriters a) where
  SerialWriters a <> SerialWriters a' = SerialWriters (HM.unionWith const a a')
instance Monoid (SerialWriters a) where
  mempty = SerialWriters mempty

instance Contravariant SerialWriters where
  contramap f sw = SerialWriters
    { _serialWritersToAtomic = fmap (\(ToAtomicFn f') -> ToAtomicFn $ f' . f)
                               (_serialWritersToAtomic sw)
    -- , _serialWritersToStream = fmap (\(ToStreamFn f') -> ToStreamFn $ f' . f)
    --                            (_serialWritersToStream sw)
    }

-- | Links a serialization method to a prefered file extension, if this is
-- relevant.
class SerializationMethod serial where
  -- | If @Just x@, @x@ should correspond to one of the keys in
  -- _serialReadersFromStream or _serialWritersToAtomic.
  getSerialDefaultExt :: serial -> Maybe FileExt
  getSerialDefaultExt _ = Nothing

-- | Tells whether some type @a@ can be serialized by some _serial_ (serialization
-- method).
class (SerializationMethod serial) => SerializesWith serial a where
  getSerialWriters :: serial -> SerialWriters a

-- | Tells whether some type @a@ can be deserialized by some _serial_
-- (serialization method).
class (SerializationMethod serial) => DeserializesWith serial a where
  getSerialReaders :: serial -> SerialReaders a

-- * Serialization to/from JSON and YAML, which both use the same intermediary
-- type, Data.Aeson.Value

-- | Has 'SerializesWith' & 'DeserializesWith' instances that permits to
-- store/load JSON and YAML files and 'A.Value's.
data JSONSerial = JSONSerial  -- ^ Expects @.json@ files by default, but supports
                              -- @.yaml@/@.yml@ files too "for free"
                | YAMLSerial  -- ^ Expects @.yaml@/@.yml@ files by default, but
                              -- supports @.json@ files too "for free"

-- | For when you want a JSON **only** or YAML **only** serialization, but tied to a
-- specific extension. It's more restrictive than 'JSONSerial' in the sense that
-- JSONSerialWithExt cannot read from values from the configuration (because in
-- the config we only have an Aeson Value, without an associated extension, so
-- we cannot know for sure this Value corresponds to the expected extension)
data JSONSerialWithExt = JSONSerialWithExt FileExt
                            -- ^ Expects files of a given extension, ONLY
                            -- formatted in JSON (YAML not provided "for free")
                       | YAMLSerialWithExt FileExt
                            -- ^ Expects files of a given extension, ONLY
                            -- formatted in YAML (JSON not provided "for free")

instance SerializationMethod JSONSerial where
  getSerialDefaultExt JSONSerial = Just "json"
  getSerialDefaultExt YAMLSerial = Just "yaml"

instance SerializationMethod JSONSerialWithExt where
  getSerialDefaultExt (JSONSerialWithExt e) = Just e
  getSerialDefaultExt (YAMLSerialWithExt e) = Just e

-- | To lazy bytestring of JSON
toAtomicJSON, toAtomicYAML
  :: ToJSON a
  => [FileExt] -> HM.HashMap (TypeRep, Maybe FileExt) (ToAtomicFn a)
toAtomicJSON exts =
  toAtomicFn (map Just exts) A.encode

-- | To lazy bytestring of YAML
toAtomicYAML exts =
  toAtomicFn (map Just exts) $ LBS.fromStrict . Y.encode

instance (ToJSON a) => SerializesWith JSONSerial a where
  getSerialWriters _srl = mempty
    { _serialWritersToAtomic =
        toAtomicFn [Nothing] A.toJSON  -- To A.Value, doesn't need an extension
        <> toAtomicJSON ["json"]
        <> toAtomicYAML ["yaml","yml"] }

instance (ToJSON a) => SerializesWith JSONSerialWithExt a where
  getSerialWriters (JSONSerialWithExt ext) = mempty
    { _serialWritersToAtomic = toAtomicJSON [ext] }
  getSerialWriters (YAMLSerialWithExt ext) = mempty
    { _serialWritersToAtomic = toAtomicYAML [ext] }

parseJSONEither :: (A.FromJSON t) => A.Value -> Either String t
parseJSONEither x = case A.fromJSON x of
  A.Success s -> Right s
  A.Error r   -> Left r
{-# INLINE parseJSONEither #-}

-- | From strict bytestring of JSON
fromAtomicJSON, fromAtomicYAML
  :: FromJSON a
  => [FileExt] -> HM.HashMap (TypeRep, Maybe FileExt) (FromAtomicFn a)
fromAtomicJSON exts =
  fromAtomicFn (map Just exts) A.eitherDecodeStrict

-- | From strict bytestring of YAML
fromAtomicYAML exts =
  fromAtomicFn (map Just exts) $
    over _Left displayException . Y.decodeEither'

-- | From a stream of strict bytestrings of JSON
fromJSONStream, fromYAMLStream
  :: FromJSON a
  => [FileExt] -> HM.HashMap (TypeRep, Maybe FileExt) (FromStreamFn a)
fromJSONStream exts = fromStreamFn (map Just exts) $ \strm -> do
  BSS.toStrict_ (BSS.fromChunks strm) >>= decodeJ
    -- TODO: Enhance this so we don't have to accumulate the whole
  where
    decodeJ x = case A.eitherDecodeStrict x of
      Right y  -> return y
      Left msg -> throwWithPrefix msg

-- | From a stream of strict bytestrings of YAML
fromYAMLStream exts = fromStreamFn (map Just exts) (decodeYAMLStream . BSS.fromChunks)

decodeYAMLStream :: (LogThrow m, FromJSON a) => BSS.ByteString m () -> m a
decodeYAMLStream strm = do
  BSS.toStrict_ strm >>= decodeY -- TODO: same than above
  where
    decodeY x = case Y.decodeEither' x of
      Right y  -> return y
      Left exc -> logAndThrowM exc

instance (FromJSON a) => DeserializesWith JSONSerial a where
  getSerialReaders _srl = mempty
    { _serialReadersFromAtomic =
        fromAtomicFn [Nothing] parseJSONEither -- From A.Value, doesn't need an
                                               -- extension
        <> fromAtomicJSON ["json"]
        <> fromAtomicYAML ["yaml","yml"]
    , _serialReadersFromStream =
        fromJSONStream ["json"]
        -- TODO: Add reading from a stream of JSON objects (which would
        -- therefore be considered a JSON array of objects?)
        <>
        fromYAMLStream ["yaml","yml"] }

instance (FromJSON a) => DeserializesWith JSONSerialWithExt a where
  getSerialReaders (JSONSerialWithExt ext) = mempty
    { _serialReadersFromAtomic = fromAtomicJSON [ext]
    , _serialReadersFromStream = fromJSONStream [ext] }
  getSerialReaders (YAMLSerialWithExt ext) = mempty
    { _serialReadersFromAtomic = fromAtomicYAML [ext]
    , _serialReadersFromStream = fromYAMLStream [ext] }


-- * Helpers to write to and from binary representations

class ToBinaryBuilder serial a where
  toBinaryBuilder :: serial -> a -> BinBuilder.Builder

-- | Recommendation: instances should implement fromLazyByteString and
-- fromByteStream whenever possible.
class FromByteStream serial a where
  fromLazyByteString :: serial -> LBS.ByteString -> Either String a
  fromLazyByteString s = fromStrictByteString s . LBS.toStrict
  fromStrictByteString :: serial -> BS.ByteString -> Either String a
  fromStrictByteString s = fromLazyByteString s . LBS.fromStrict
  fromByteStream :: (LogThrow m) => serial -> BSS.ByteString m () -> m a
  fromByteStream s bss = do
    bs <- BSS.toLazy_ bss  -- This default implementation is stricter than
                                 -- it needs to be
    case fromLazyByteString s bs of
      Left msg -> throwWithPrefix msg
      Right y -> return y

getSerialWriters_ToBinaryBuilder
  :: (SerializationMethod srl, ToBinaryBuilder srl a) => srl -> SerialWriters a
getSerialWriters_ToBinaryBuilder srl = mempty
  { _serialWritersToAtomic =
    toAtomicFn [getSerialDefaultExt srl] $
      BinBuilder.toLazyByteString . toBinaryBuilder srl }

getSerialReaders_FromByteStream
  :: (SerializationMethod srl, FromByteStream srl a) => srl -> SerialReaders a
getSerialReaders_FromByteStream srl = mempty
    { _serialReadersFromStream =
        fromStreamFn [getSerialDefaultExt srl] (fromByteStream srl . BSS.fromChunks)
    , _serialReadersFromAtomic =  -- From strict bytestring
        fromAtomicFn [getSerialDefaultExt srl] (fromStrictByteString srl)
    }

-- * Serialization to/from CSV

-- | Data with header not known in advance, that can be converted to/from CSV,
-- keeping track of the header
data Tabular a = Tabular
  { tabularHeader :: Maybe [T.Text]
  , tabularData :: a }
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Data that can be converted to/from CSV, with previous knowledge of the
-- headers
newtype Records a = Records { fromRecords :: a }

instance (Show a) => Show (Records a) where
  show = show . fromRecords

instance (ToJSON a) => ToJSON (Records a) where
  toJSON = toJSON . fromRecords

instance (FromJSON a) => FromJSON (Records a) where
  parseJSON = fmap Records . parseJSON

-- | Can serialize and deserialize any @Tabular a@ from a CSV file
data CSVSerial = CSVSerial
  { csvSerialExt       :: FileExt
    -- ^ The extension to use (csv, tsv, txt, etc.)
  , csvSerialHasHeader :: Bool
    -- ^ The csv file contains a header (to skip or to read/write). Must be True
    -- if we want to read 'Records' from it
  , csvSerialDelimiter :: Char
    -- ^ The character (@,@, @\t@, etc.) to use as a field delimiter.
  }

instance SerializationMethod CSVSerial where
  getSerialDefaultExt = Just . csvSerialExt

instance (Foldable f, Csv.ToRecord a) => ToBinaryBuilder CSVSerial (Tabular (f a)) where
  toBinaryBuilder (CSVSerial _ hasHeader delim) (Tabular mbHeader dat) =
    mbAddHeader $ foldMap encField dat
    where
      mbAddHeader | hasHeader = maybe id (<>) (encHeader <$> mbHeader)
                  | otherwise = id
      encodeOpts = Csv.defaultEncodeOptions {Csv.encDelimiter = fromIntegral $ ord delim}
      encHeader = CsvBuilder.encodeRecordWith encodeOpts
      encField = CsvBuilder.encodeRecordWith encodeOpts

instance (Foldable f, Csv.ToNamedRecord a, Csv.DefaultOrdered a)
  => ToBinaryBuilder CSVSerial (Records (f a)) where
  toBinaryBuilder (CSVSerial _ hasHeader delim) (Records dat) =
    mbAddHeader $ foldMap encField dat
    where
      mbAddHeader | hasHeader = (encHeader (Csv.headerOrder (undefined :: a)) <>)
                  | otherwise = id
      encodeOpts = Csv.defaultEncodeOptions {Csv.encDelimiter = fromIntegral $ ord delim}
      encHeader = CsvBuilder.encodeHeaderWith encodeOpts
      encField = CsvBuilder.encodeDefaultOrderedNamedRecordWith encodeOpts

instance (Csv.FromRecord a) => FromByteStream CSVSerial (Tabular (V.Vector a)) where
  fromLazyByteString (CSVSerial _ hasHeader delim) bs =
    Tabular Nothing <$>  -- TODO: parse header
    Csv.decodeWith decOpts (if hasHeader then Csv.HasHeader else Csv.NoHeader) bs
    where
      decOpts = Csv.defaultDecodeOptions {Csv.decDelimiter=fromIntegral $ ord delim}

instance (Csv.FromNamedRecord a) => FromByteStream CSVSerial (Records (V.Vector a)) where
  fromLazyByteString (CSVSerial _ hasHeader delim) bs =
    if not hasHeader then error "CANNOT USE ColNamed on CSV files without headers"
    else do
      (_, v) <- Csv.decodeByNameWith decOpts bs
      return $ Records v
    where
      decOpts = Csv.defaultDecodeOptions {Csv.decDelimiter=fromIntegral $ ord delim}

instance (Foldable f, Csv.ToRecord a) => SerializesWith CSVSerial (Tabular (f a)) where
  getSerialWriters = getSerialWriters_ToBinaryBuilder

instance (Foldable f, Csv.ToNamedRecord a, Csv.DefaultOrdered a)
  => SerializesWith CSVSerial (Records (f a)) where
  getSerialWriters = getSerialWriters_ToBinaryBuilder

instance (Csv.FromRecord a) => DeserializesWith CSVSerial (Tabular (V.Vector a)) where
  getSerialReaders = getSerialReaders_FromByteStream

instance (Csv.FromNamedRecord a) => DeserializesWith CSVSerial (Records (V.Vector a)) where
  getSerialReaders = getSerialReaders_FromByteStream

-- instance (Csv.ToNamedRecord a, Foldable f) => 

-- TODO: recover header when deserializing CSV (which cassava doesn't return)
-- decodeTabular :: Bool -> Char -> LBS.ByteString -> Either String (Tabular (V.Vector a))
-- decodeTabular hasHeader delim bs =
--   mbHeader <- if hasHeader
--     then AttoL.parse (CsvParser.header delim') bs
--     else return Nothing
--   where
--     delim' = fromIntegral $ ord delim
--     decOpts = Csv.defaultDecodeOptions {Csv.decDelimiter=delim'}

-- * "Serialization" to/from bytestrings

-- | ByteStringSerial is just a reader of strict ByteStrings and writer of lazy
-- ByteStrings. It's the simplest SerializationMethod possible
newtype ByteStringSerial = ByteStringSerial { bsSerialSpecificExt :: Maybe FileExt }

instance SerializationMethod ByteStringSerial where
  getSerialDefaultExt (ByteStringSerial ext) = ext

instance SerializesWith ByteStringSerial LBS.ByteString where
  getSerialWriters (ByteStringSerial ext) = mempty
    { _serialWritersToAtomic = toAtomicFn [ext] id }
    -- TODO: Add base64 encoding so it can be read/written from/to JSON strings
    -- too

-- We only deserialize *strict* bytestrings, in order not to hide the fact that
-- the data must be accumulated from the stream we read if you want to break
-- away from it

instance DeserializesWith ByteStringSerial BS.ByteString where
  getSerialReaders (ByteStringSerial ext) = mempty
    { _serialReadersFromAtomic =
        fromAtomicFn [ext] Right
    , _serialReadersFromStream =
        fromStreamFn [ext] S.mconcat_ }

-- * Serialization to/from plain text

-- | Can read from text files or raw input strings in the pipeline configuration
-- file. Should be used only for small files or input strings. If we should
-- accept only some extension, specify it. Else just use Nothing.
newtype PlainTextSerial = PlainTextSerial { plainTextSerialSpecificExt :: Maybe FileExt }

instance SerializationMethod PlainTextSerial where
  getSerialDefaultExt (PlainTextSerial ext) = ext

instance SerializesWith PlainTextSerial T.Text where
  getSerialWriters (PlainTextSerial ext) = mempty
    { _serialWritersToAtomic =
      toAtomicFn [Nothing] (\t -> LT.fromChunks [t]) -- To lazy text
      <> toAtomicFn [ext] (\t -> LTE.encodeUtf8 $ LT.fromChunks [t]) -- To lazy bytestring
      <> toAtomicFn [ext] toJSON  -- To A.Value
    }

instance SerializesWith PlainTextSerial LT.Text where
  getSerialWriters (PlainTextSerial ext) = mempty
    { _serialWritersToAtomic =
      toAtomicFn [Nothing] id -- To lazy text
      <> toAtomicFn [ext] LTE.encodeUtf8 -- To lazy bytestring
      <> toAtomicFn [ext] toJSON  -- To A.Value
    }

instance DeserializesWith PlainTextSerial T.Text where
  getSerialReaders (PlainTextSerial ext) = mempty
    { _serialReadersFromAtomic =
        fromAtomicFn [Nothing] Right
        <> fromAtomicFn [ext] parseJSONEither
        <> fromAtomicFn [ext] (Right . TE.decodeUtf8)
    , _serialReadersFromStream =
        fromStreamFn [ext] S.mconcat_
        <>
        fromStreamFn [ext] (fmap TE.decodeUtf8 . S.mconcat_)
    }

-- * Serialization of options

-- | Contains any set of options that should be exposed via the CLI
data RecOfOptions field where
  RecOfOptions :: (Typeable rs, RecordUsableWithCLI rs) => Rec field rs -> RecOfOptions field

type DocRecOfOptions = RecOfOptions DocField

-- | A serialization method used for options which can have a default value,
-- that can be exposed through the configuration.
data OptionsSerial a = forall rs. (Typeable rs, RecordUsableWithCLI rs)
                   => OptionsSerial (a -> DocRec rs) (DocRec rs -> a)
instance SerializationMethod (OptionsSerial a)
instance SerializesWith (OptionsSerial a) a where
  getSerialWriters (OptionsSerial f _) = mempty
    { _serialWritersToAtomic =
        toAtomicFn [Nothing] (RecOfOptions . f) }
instance DeserializesWith (OptionsSerial a) a where
  getSerialReaders (OptionsSerial _ (f :: DocRec rs -> a)) = mempty
    { _serialReadersFromAtomic =
        let conv :: DocRecOfOptions -> Either String a
            conv (RecOfOptions r) = case cast r of
              Just r' -> Right $ f r'
              Nothing -> Left "OptionsSerial: _serialReadersFromAtomic: Not the right fields"
        in fromAtomicFn [Nothing] conv }


-- * Combining serializers and deserializers into one structure

-- | Can serialize @a@ and deserialize @b@.
data SerialsFor a b = SerialsFor
  { _serialWriters        :: SerialWriters a
  , _serialReaders        :: SerialReaders b
  , _serialDefaultExt     :: First FileExt
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

-- | Changes the serialization function used by default
setDefaultSerial :: FileExt -> SerialsFor a b -> SerialsFor a b
setDefaultSerial = set serialDefaultExt . First . Just

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


-- * Retrieve conversion functions from a 'SerialsFor' @a@ @b@

-- | Tries to get a conversion function to some type @i@
getToAtomicFn :: forall i a b. (Typeable i) => SerialsFor a b -> Maybe (a -> i)
getToAtomicFn ser = do
  ToAtomicFn (f :: a -> i') <-
    HM.lookup (typeOf (undefined :: i),Nothing) (ser ^. serialWriters . serialWritersToAtomic)
  case eqT :: Maybe (i' :~: i) of
    Just Refl -> return f
    Nothing -> error $ "getToAtomicFn: Some conversion function isn't properly indexed. Should not happen"

-- | Tries to get a conversion function from some type @i@
getFromAtomicFn :: forall i a b. (Typeable i) => SerialsFor a b -> Maybe (FromAtomicFn' i b)
getFromAtomicFn ser = do
  FromAtomicFn (f :: FromAtomicFn' i' b) <-
    HM.lookup (typeOf (undefined :: i),Nothing) (ser ^. serialReaders . serialReadersFromAtomic)
  case eqT :: Maybe (i' :~: i) of
    Just Refl -> return f
    Nothing -> error $ "getFromAtomicFn: Some conversion function isn't properly indexed. Should not happen"


-- * Serialization for compressed formats

-- | Wraps all the functions in the serial so for each serial (extension) @xxx@
-- supported, we know also support @xxxzlib@. Doesn't change the default
-- extension
addZlibSerials :: SerialsFor a b -> SerialsFor a b
addZlibSerials = over serialWriters (over serialWritersToAtomic editTA)
              . over serialReaders (over serialReadersFromAtomic editFA
                                  . over serialReadersFromStream editFS)
  where
    editTA hm = (hm <>) $ mconcat $ flip map (allToAtomicFnsWithType hm) $
      \(ext, f) ->
        toAtomicFn [Just $ ext <> "zlib"] $ Zlib.compress . f  -- Lazy bytestring
    editFA hm = (hm <>) $ mconcat $ flip map (allFromAtomicFnsWithType hm) $
      \(ext, f) ->
        fromAtomicFn [Just $ ext <> "zlib"] $
          f . LBS.toStrict . Zlib.decompress . LBS.fromStrict  -- Strict bytestring
    editFS hm = (hm <>) $ mconcat $ flip map (allFromStreamFnsWithType hm) $
      \(ext, FromStreamFn'' f) ->
        fromStreamFn [Just $ ext <> "zlib"] $
          f . BSS.toChunks . SZip.decompress SZip.defaultWindowBits . BSS.fromChunks

-- | Adds warnings when deserializing values /from a stream/
addDeserialWarnings :: (b -> [String]) -> SerialsFor a b -> SerialsFor a b
addDeserialWarnings f = serialReaders . serialReadersFromStream . traversed %~ addW
  where
    addW (FromStreamFn g) = FromStreamFn $ \s -> do
      a <- g s
      let warnings = f a
      mapM_ (logFM WarningS . logStr) warnings
      return a


-- -- | Traverses to the repetition keys stored in the access functions of a
-- -- 'SerialsFor'
-- serialsRepetitionKeys :: Traversal' (SerialsFor a b) [LocVariable]
-- serialsRepetitionKeys f (SerialsFor writers readers ext rk) =
--   rebuild <$> (serialWritersToOutputFile . traversed . writeToLocRepetitionKeys) f writers
--           <*> (serialReadersFromInputFile . traversed . readFromLocRepetitionKeys) f readers
--   where
--     rebuild w r = SerialsFor w r ext rk
