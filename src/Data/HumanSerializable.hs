{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Data.HumanSerializable
  ( ToHuman(..), OfHuman(..), Serializable
  , RetrievingError(..)
  , persistAtLoc
  , loadFromLoc
  ) where

import           Control.Monad.Catch
import qualified Data.Aeson           as JSON
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Locations       as Loc
import qualified Data.Text            as T


class ToHuman a where
  encode :: a -> LBS.ByteString
  default encode :: JSON.ToJSON a => a -> LBS.ByteString
  encode = JSON.encode

class OfHuman a where
  decode :: LBS.ByteString -> Either T.Text a
  default decode :: JSON.FromJSON a => LBS.ByteString -> Either T.Text a
  decode x =
    case JSON.eitherDecode x of
      Left msg  -> Left $ T.pack msg
      Right elt -> Right elt

-- | Class of values which can be serialized in a human-readable form
-- (json, csv, whatever…)
type Serializable a = (ToHuman a, OfHuman a)

data RetrievingError
  = FileReadError Loc.Error
  | DecodingError Loc.Loc T.Text

instance Exception RetrievingError

instance Show RetrievingError where
  show (FileReadError loc) = "Impossible to read file " <> show loc
  show (DecodingError loc msg) =
    "Error while decoding file " <> show loc <> ": " <> T.unpack msg


-- | Write persistable data at some location
persistAtLoc
  :: (Loc.LocationMonad m, ToHuman a)
  => a
  -> Loc.Loc
  -> m ()
persistAtLoc x loc = do
  Loc.writeLazyByte loc $ encode x
  Loc.logMsg $ "The file " <> show loc <> " has been saved"

-- | Retrieve persistable data from some location
loadFromLoc
  :: forall m a.
     (Loc.LocationMonad m, OfHuman a)
  => Loc.Loc
  -> m a
loadFromLoc origFile = do
  Loc.readLazyByte origFile
  >>= withReadError
  >>= decodeWithLoc origFile
  where
    withReadError :: Either Loc.Error b -> m b
    withReadError (Right x)  = return x
    withReadError (Left err) = throwM $ FileReadError err

    decodeWithLoc loc x = case decode x of
      Right y  -> return y
      Left err -> throwM $ DecodingError loc err
