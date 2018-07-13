{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Control.Logger where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Binary            (Binary)
import           Data.Default           (Default, def)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Data.Typeable          (Typeable)
import           GHC.Generics           (Generic)

-- | Control for the verbosity of the program
data Verbosity
  = Silent
  | Quiet
  | Normal
  deriving (Eq, Ord, Show, Bounded, Enum, Generic, Typeable)

instance Binary Verbosity
instance FromJSON Verbosity
instance ToJSON Verbosity
instance Default Verbosity where def = Normal

-- | Displays a message on stdout if the verbosity level is big enough
log
  :: MonadIO m
  => Verbosity -- Current verbosity
  -> Verbosity -- Minimal verbosity at which to show the message
  -> T.Text -- Message to show
  -> m ()
log curVerb msgVerb =
  if curVerb >= msgVerb
  then liftIO . TIO.putStrLn
  else const (pure ())
