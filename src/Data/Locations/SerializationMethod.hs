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
{-# OPTIONS_GHC -Wall #-}

module Data.Locations.SerializationMethod where

import           Control.Applicative
import           Control.Monad.Catch
import           Data.Aeson                   as A
import           Data.Binary
import           Data.Default
import           Data.List.NonEmpty           (NonEmpty (..), toList)
import           Data.Locations.Loc           as Loc
import           Data.Locations.LocationMonad as Loc
import qualified Data.Map                     as Map
import           Data.Profunctor
import           Data.Representable
import qualified Data.Text                    as T
import           Data.Void
import           GHC.Generics
import qualified Katip                        as K


-- | Some locs will allow several serialization methods to be used, but often we
-- will just LocDefault (JSON for local files and S3 objects). They have some
-- priority order.
--
-- Currently, this type is used mostly for defining file extensions, and should
-- be removed in the future. See the newest 'SerializationMethod' class that
-- handles the serialization/deserialization code per se.
data SerialMethod =
  LocDefault | JSON | CSV | Markdown | PDF | BinaryObj | SQLTableData
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, Binary)

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


class SerializationMethod serial where
  canSerializeAtLoc :: serial -> Loc -> Bool
  associatedFileType :: serial -> SerialMethod  -- only temporary, to ease
                                                      -- transition

-- | Tells whether some type @a@ can be serialized in some location with some
-- serialization method @serial@.
class (SerializationMethod serial) => SerializesWith serial a | serial -> a where
  persistAtLoc :: (LocationMonad m) => serial -> a -> Loc -> m ()

class (SerializationMethod serial) => DeserializesWith serial a | serial -> a where
  loadFromLoc  :: (LocationMonad m) => serial -> Loc -> m a

-- | Writes a file and logs the information about the write
persistAndLog :: (SerializesWith serial a, LocationMonad m, K.KatipContext m)
              => serial -> a -> Loc -> m ()
persistAndLog s a l = do
  persistAtLoc s a l
  K.logFM K.NoticeS $ K.logStr $ "Wrote file '" ++ show l ++ "'"

data VoidSerial = VoidSerial

instance SerializationMethod VoidSerial where
  canSerializeAtLoc _ _ = True
  associatedFileType _  = LocDefault

instance SerializesWith VoidSerial Void where
  persistAtLoc _ _ _ = return ()

instance DeserializesWith VoidSerial () where
  loadFromLoc _ _ = return ()

-- | Has 'SerializesWith' & 'DeserializesWith' instances that permits to
-- store/load JSON files through a 'LocationMonad'
data JSONSerial a = JSONSerial

instance SerializationMethod (JSONSerial a) where
  canSerializeAtLoc _ _ = True
  associatedFileType _ = JSON

instance (ToJSON a) => SerializesWith (JSONSerial a) a where
  persistAtLoc _ x loc = do
    Loc.writeLazyByte loc $ A.encode x

instance (FromJSON a) => DeserializesWith (JSONSerial a) a where
  loadFromLoc _ loc =
    Loc.readLazyByte loc >>= withReadError >>= decodeWithLoc
   where
    withReadError (Right x)  = return x
    withReadError (Left err) = throwM $ FileReadError err
    decodeWithLoc x = case A.eitherDecode x of
      Right y  -> return y
      Left msg -> throwM $ DecodingError loc $ T.pack msg

-- | The crudest SerializationMethod there is. Works only for 'T.Text'. Should
-- be used only for small files.
newtype PlainTextSerial = PlainTextSerial { textSerialExt :: T.Text }

instance SerializationMethod PlainTextSerial where
  canSerializeAtLoc _ _ = True
  associatedFileType (PlainTextSerial ext) = case fromTextRepr ext of
    Just ft -> ft
    Nothing -> error $ "Custom extension " ++ T.unpack ext ++ " isn't supported for now."

instance SerializesWith PlainTextSerial T.Text where
  persistAtLoc _ x loc = writeText loc x

instance DeserializesWith PlainTextSerial T.Text where
  loadFromLoc _ loc = do
    res <- readText loc
    case res of
      Left err -> throwM err
      Right r  -> return r

-- | A SerializationMethod that's meant to be used just locally, for one datatype and one
-- file
data CustomPureSerial a =
  CustomPureSerial SerialMethod (forall m. (LocationMonad m) => a -> Loc -> m ())

instance SerializationMethod (CustomPureSerial a) where
  canSerializeAtLoc _ _ = True
  associatedFileType (CustomPureSerial ft _) = ft
instance SerializesWith (CustomPureSerial a) a where
  persistAtLoc (CustomPureSerial _ f) = f

-- | A DeserializationMethod that's meant to be used just locally, for one datatype and one
-- file
data CustomPureDeserial a =
  CustomPureDeserial SerialMethod (forall m. (LocationMonad m) => Loc -> m a)

instance SerializationMethod (CustomPureDeserial a) where
  canSerializeAtLoc _ _ = True
  associatedFileType (CustomPureDeserial ft _) = ft
instance DeserializesWith (CustomPureDeserial a) a where
  loadFromLoc (CustomPureDeserial _ f) = f

-- * Grouping 'SerializationMethod's together, to indicate all the possible
-- serials for a type of data

data SomeSerialFor a
  = forall s b. (SerializesWith s b) => SomeSerial s (a -> b)

contramapSomeSerial :: (a1 -> a2)
                    -> SomeSerialFor a2 -> SomeSerialFor a1
contramapSomeSerial f' (SomeSerial s f) = SomeSerial s (f . f')

data SomeDeserialFor a
  = forall s b. (DeserializesWith s b) => SomeDeserial s (b -> a)

instance Functor SomeDeserialFor where
  fmap f' (SomeDeserial s f) = SomeDeserial s (f' . f)

-- | Can serialize @a@ and deserialize @b@.
data SerialsFor a b = SerialsFor (NonEmpty (SomeSerialFor a)) (NonEmpty (SomeDeserialFor b))

-- | Can serialize and deserialize @a@. Use 'dimap' to transform it
type BidirSerials a = SerialsFor a a

-- | Can only serialize @a@. Use 'lmap' to transform it.
type PureSerials a = SerialsFor a ()

-- | Can only deserialize @a@. Use 'rmap' to transform it.
type PureDeserials a = SerialsFor Void a

instance Profunctor SerialsFor where
  dimap f g (SerialsFor sers desers) =
    SerialsFor (fmap (contramapSomeSerial f) sers) (fmap (fmap g) desers)

instance Semigroup (SerialsFor a b) where
  SerialsFor (s:|ss) (d:|dd) <> SerialsFor (s':|ss') (d':|dd') =
    SerialsFor (s :| ss++[s']++ss') (d :| dd ++[d']++dd')

voidSerial :: NonEmpty (SomeSerialFor Void)
voidSerial = SomeSerial VoidSerial id :| []

voidDeserial :: NonEmpty (SomeDeserialFor ())
voidDeserial = SomeDeserial VoidSerial id :| []

-- | Packs together ways to serialize and deserialize some data @a@
someBidirSerial :: (SerializesWith s a, DeserializesWith s a) => s -> BidirSerials a
someBidirSerial s = SerialsFor (SomeSerial s id :| []) (SomeDeserial s id :| [])

makeBidir :: PureSerials a -> PureDeserials a -> BidirSerials a
makeBidir (SerialsFor sers _) (SerialsFor _ desers) = SerialsFor sers desers

-- | Packs together ways to serialize some data @a@
somePureSerial :: (SerializesWith s a) => s -> PureSerials a
somePureSerial s = SerialsFor (SomeSerial s id :| []) voidDeserial

-- | Packs together ways to deserialize and deserialize some data @a@
somePureDeserial :: (DeserializesWith s a) => s -> PureDeserials a
somePureDeserial s = SerialsFor voidSerial (SomeDeserial s id :| [])

eraseSerials :: SerialsFor a b -> PureDeserials b
eraseSerials (SerialsFor _ desers) = SerialsFor voidSerial desers

eraseDeserials :: SerialsFor a b -> PureSerials a
eraseDeserials (SerialsFor sers _) = SerialsFor sers voidDeserial


-- | Builds a custom SerializationMethod (ie. which cannot be used for
-- deserialization) which is just meant to be used for one datatype.
customPureSerial
  :: T.Text   -- ^ The file extension associated to this SerializationMethod
  -> (forall m. (LocationMonad m) => a -> Loc -> m ())
  -> PureSerials a
customPureSerial ext f =
  case fromTextRepr ext of
    Nothing ->
      error $ "customPureSerial: " ++ T.unpack ext ++ " isn't associated with any SerialMethod"
    Just ft ->
      somePureSerial $ CustomPureSerial ft f

-- | Builds a custom SerializationMethod (ie. which cannot be used for
-- deserialization) which is just meant to be used for one datatype.
customPureDeserial
  :: T.Text   -- ^ The file extension associated to this SerializationMethod
  -> (forall m. (LocationMonad m) => Loc -> m a)
  -> PureDeserials a
customPureDeserial ext f =
  case fromTextRepr ext of
    Nothing ->
      error $ "customPureDeserial: " ++ T.unpack ext ++ " isn't associated with any SerialMethod"
    Just ft ->
      somePureDeserial $ CustomPureDeserial ft f


-- * Functions for compatiblity with part of the API still using 'SerialMethod'.

indexPureSerialsByFileType :: SerialsFor a b -> Map.Map SerialMethod (SomeSerialFor a)
indexPureSerialsByFileType (SerialsFor sers _) = Map.fromList . toList $
  fmap (\s@(SomeSerial s' _) -> (associatedFileType s', s)) sers

indexPureDeserialsByFileType :: SerialsFor a b -> Map.Map SerialMethod (SomeDeserialFor b)
indexPureDeserialsByFileType (SerialsFor _ desers) = Map.fromList . toList $
  fmap (\s@(SomeDeserial s' _) -> (associatedFileType s', s)) desers

firstPureSerialFileType :: SerialsFor a b -> SerialMethod
firstPureSerialFileType (SerialsFor (SomeSerial s _ :| _) _) = associatedFileType s

firstPureDeserialFileType :: SerialsFor a b -> SerialMethod
firstPureDeserialFileType (SerialsFor _ (SomeDeserial s _ :| _)) = associatedFileType s
