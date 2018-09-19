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
  = forall s. (SerializesWith s a) => SomeSerial s
data SomeDeserialFor a
  = forall s. (DeserializesWith s a) => SomeDeserial s

data SerialsFor a = SerialsFor [SomeSerialFor a] [SomeDeserialFor a]

instance Semigroup (SerialsFor a) where
  (SerialsFor x y) <> (SerialsFor x' y') = SerialsFor (x++x') (y++y')
instance Monoid (SerialsFor a) where
  mempty = SerialsFor [] []

somePureSerial :: (SerializesWith s a) => s -> SerialsFor a
somePureSerial s = SerialsFor [SomeSerial s] []

somePureDeserial :: (DeserializesWith s a) => s -> SerialsFor a
somePureDeserial s = SerialsFor [] [SomeDeserial s]

someSerial :: (SerializesWith s a, DeserializesWith s a) => s -> SerialsFor a
someSerial s = SerialsFor [SomeSerial s] [SomeDeserial s]

indexSerialsByFileType :: SerialsFor a -> Map.Map SerialMethod (SomeSerialFor a)
indexSerialsByFileType (SerialsFor l _) = Map.fromList $
  map (\s@(SomeSerial s') -> (associatedFileType s', s)) l

indexDeserialsByFileType :: SerialsFor a -> Map.Map SerialMethod (SomeDeserialFor a)
indexDeserialsByFileType (SerialsFor _ l) = Map.fromList $
  map (\s@(SomeDeserial s') -> (associatedFileType s', s)) l

class HasSerializationMethods a where
  allSerialsFor :: a -> SerialsFor a
