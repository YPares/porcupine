{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# OPTIONS_GHC -Wall #-}

module Data.SerializationMethod where

import           Control.Applicative
import           Control.Monad.Catch
import           Data.Aeson                   as A
import           Data.Binary
import           Data.Default
import           Data.Functor.Contravariant
import           Data.Locations.Loc           as Loc
import           Data.Locations.LocationMonad as Loc
import qualified Data.Map                     as Map
import           Data.Representable
import qualified Data.Text                    as T
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


class (Default serial) => SerializationMethod serial where
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

instance Default JSONSerial where
  def = JSONSerial

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

-- | Groups together ways to serialize some type @a@, either directly or by
-- embedding transformations through calls to 'contramap'.
data SerialsFor a = SerialsFor (SomeSerialFor a) [SomeSerialFor a]

instance Contravariant SerialsFor where
  contramap f (SerialsFor s1 sers) =
    SerialsFor (contramap f s1) (map (contramap f) sers)

instance Semigroup (SerialsFor a) where
  (SerialsFor s1 ss1) <> (SerialsFor s2 ss2) = SerialsFor s1 (ss1 ++ [s2] ++ ss2)

-- | Groups together ways to deserialize some type @a@, either directly or by
-- embedding transformations through calls to 'fmap'.
data DeserialsFor a = DeserialsFor (SomeDeserialFor a) [SomeDeserialFor a]

instance Functor DeserialsFor where
  fmap f (DeserialsFor d1 desers) =
    DeserialsFor (fmap f d1) (map (fmap f) desers)

instance Semigroup (DeserialsFor a) where
  (DeserialsFor s1 ss1) <> (DeserialsFor s2 ss2) = DeserialsFor s1 (ss1 ++ [s2] ++ ss2)

someSerial :: (SerializesWith s a) => s -> SerialsFor a
someSerial s = SerialsFor (SomeSerial s id) []

someDeserial :: (DeserializesWith s a) => s -> DeserialsFor a
someDeserial s = DeserialsFor (SomeDeserial s id) []

class HasSerializationMethods a where
  allSerialsFor :: a -> SerialsFor a
  allDeserialsFor :: a -> DeserialsFor a


-- * Functions for compatiblity with part of the API still using 'SerialMethod'.

indexSerialsByFileType :: SerialsFor a -> Map.Map SerialMethod (SomeSerialFor a)
indexSerialsByFileType (SerialsFor l1 ll) = Map.fromList $
  map (\s@(SomeSerial s' _) -> (associatedFileType s', s)) (l1:ll)

indexDeserialsByFileType :: DeserialsFor a -> Map.Map SerialMethod (SomeDeserialFor a)
indexDeserialsByFileType (DeserialsFor l1 ll) = Map.fromList $
  map (\s@(SomeDeserial s' _) -> (associatedFileType s', s)) (l1:ll)

firstSerialsFileType :: SerialsFor a -> SerialMethod
firstSerialsFileType (SerialsFor (SomeSerial s _) _) = associatedFileType s

firstDeserialsFileType :: DeserialsFor a -> SerialMethod
firstDeserialsFileType (DeserialsFor (SomeDeserial s _) _) = associatedFileType s
