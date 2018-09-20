{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# OPTIONS_GHC -Wall #-}

module Data.SerializationMethod where

import           Control.Applicative
import           Control.Monad.Catch
import           Data.Aeson                   as A
import           Data.Binary
import           Data.Default
import           Data.Functor.Contravariant
import           Data.List.NonEmpty           (NonEmpty (..), toList)
import           Data.Locations.Loc           as Loc
import           Data.Locations.LocationMonad as Loc
import qualified Data.Map                     as Map
import           Data.Representable
import qualified Data.Text                    as T
import           Data.Type.Bool
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
class (SerializationMethod serial) => SerializesWith serial a where
  persistAtLoc :: (LocationMonad m) => serial -> a -> Loc -> m ()

class (SerializationMethod serial) => DeserializesWith serial a where
  loadFromLoc  :: (LocationMonad m) => serial -> Loc -> m a

-- | Writes a file and logs the information about the write
persistAndLog :: (SerializesWith serial a, LocationMonad m, K.KatipContext m)
              => serial -> a -> Loc -> m ()
persistAndLog s a l = do
  persistAtLoc s a l
  K.logFM K.NoticeS $ K.logStr $ "Wrote file '" ++ show l ++ "'"

-- | Has 'SerializesWith' & 'DeserializesWith' instances that permits to
-- store/load JSON files through a 'LocationMonad'
data JSONSerial = JSONSerial

instance SerializationMethod JSONSerial where
  canSerializeAtLoc _ _ = True
  associatedFileType _ = JSON

instance (ToJSON a) => SerializesWith JSONSerial a where
  persistAtLoc _ x loc = do
    Loc.writeLazyByte loc $ A.encode x

instance (FromJSON a) => DeserializesWith JSONSerial a where
  loadFromLoc _ loc =
    Loc.readLazyByte loc >>= withReadError >>= decodeWithLoc
   where
    withReadError (Right x)  = return x
    withReadError (Left err) = throwM $ FileReadError err
    decodeWithLoc x = case A.eitherDecode x of
      Right y  -> return y
      Left msg -> throwM $ DecodingError loc $ T.pack msg

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

instance Contravariant SomeSerialFor where
  contramap f' (SomeSerial s f) = SomeSerial s (f . f')

data SomeDeserialFor a
  = forall s b. (DeserializesWith s b) => SomeDeserial s (b -> a)

instance Functor SomeDeserialFor where
  fmap f' (SomeDeserial s f) = SomeDeserial s (f' . f)

-- | A list-like type that contains at type level whether it's empty or not
data TList b a where
  TList :: NonEmpty a -> TList 'True a
  TNull :: TList 'False a

tlistConcat :: TList a t -> TList b t -> TList (a || b) t
tlistConcat TNull TNull = TNull
tlistConcat (TList ne) TNull = TList ne
tlistConcat TNull (TList ne) = TList ne
tlistConcat (TList (a1 :| aa)) (TList (b1 :| bb)) =
  TList $ a1 :| (aa ++ [b1] ++ bb)

type family Fst (tup :: (Bool,Bool)) where
  Fst '(a, b) = a
type family Snd (tup :: (Bool,Bool)) where
  Snd '(a, b) = b

type WritableAndReadable = '( 'True, 'True )
type Writable (r::Bool) = '( 'True, r )
type Readable (w::Bool) = '( w, 'True )
type WritableOnly = Writable 'False
type ReadableOnly = Readable 'False

-- | Groups together ways to serialize/deserialize some type @a@, either
-- directly or by embedding transformations through calls to 'contramap'. @rw@
-- is a Bool tuple, but for readability it is supposed to be one of 'Writable t'
-- (@t@ being left unspecified), 'Readable t', 'WritableAndReadable',
-- 'WritableOnly' or 'ReadableOnly'.
data SerialsFor rw a =
  SerialsFor (TList (Fst rw) (SomeSerialFor a)) (TList (Snd rw) (SomeDeserialFor a))

instance Contravariant (SerialsFor WritableOnly) where
  contramap f (SerialsFor (TList sers) TNull) =
    SerialsFor (TList $ fmap (contramap f) sers) TNull

instance Functor (SerialsFor ReadableOnly) where
  fmap f (SerialsFor TNull (TList desers)) =
    SerialsFor TNull (TList $ fmap (fmap f) desers)

-- | Combine two lists of serialization/deserialization methods for @a@.
addSerials :: SerialsFor '(w1,r1) a -> SerialsFor '(w2,r2) a -> SerialsFor '(w1||w2, r1||r2) a
addSerials (SerialsFor s d) (SerialsFor s' d') =
  SerialsFor (tlistConcat s s') (tlistConcat d d')

-- | Packs together ways to serialize and deserialize some data @a@
someSerials :: (SerializesWith s a, DeserializesWith s a) => s -> SerialsFor WritableAndReadable a
someSerials s = somePureSerial s `addSerials` somePureDeserial s

-- | Packs together ways to serialize some data @a@
somePureSerial :: (SerializesWith s a) => s -> SerialsFor WritableOnly a
somePureSerial s = SerialsFor (TList $ SomeSerial s id :| []) TNull

-- | Packs together ways to deserialize and deserialize some data @a@
somePureDeserial :: (DeserializesWith s a) => s -> SerialsFor ReadableOnly a
somePureDeserial s = SerialsFor TNull (TList $ SomeDeserial s id :| [])

-- | Builds a custom SerializationMethod (ie. which cannot be used for
-- deserialization) which is just meant to be used for one datatype.
customPureSerial
  :: T.Text   -- ^ The file extension associated to this SerializationMethod
  -> (forall m. (LocationMonad m) => a -> Loc -> m ())
  -> SerialsFor WritableOnly a
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
  -> SerialsFor ReadableOnly a
customPureDeserial ext f =
  case fromTextRepr ext of
    Nothing ->
      error $ "customPureDeserial: " ++ T.unpack ext ++ " isn't associated with any SerialMethod"
    Just ft ->
      somePureDeserial $ CustomPureDeserial ft f


class HasSerializationMethods rw a where
  allSerialsFor :: a -> SerialsFor rw a


-- * Functions for compatiblity with part of the API still using 'SerialMethod'.

indexPureSerialsByFileType :: SerialsFor (Writable t) a -> Map.Map SerialMethod (SomeSerialFor a)
indexPureSerialsByFileType (SerialsFor (TList sers) _) = Map.fromList . toList $
  fmap (\s@(SomeSerial s' _) -> (associatedFileType s', s)) sers

indexPureDeserialsByFileType :: SerialsFor (Readable t) a -> Map.Map SerialMethod (SomeDeserialFor a)
indexPureDeserialsByFileType (SerialsFor _ (TList desers)) = Map.fromList . toList $
  fmap (\s@(SomeDeserial s' _) -> (associatedFileType s', s)) desers

firstPureSerialFileType :: SerialsFor (Writable t) a -> SerialMethod
firstPureSerialFileType (SerialsFor (TList (SomeSerial s _ :| _)) _) = associatedFileType s

firstPureDeserialFileType :: SerialsFor (Readable t) a -> SerialMethod
firstPureDeserialFileType (SerialsFor _ (TList (SomeDeserial s _ :| _))) = associatedFileType s
