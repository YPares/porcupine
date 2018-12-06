{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE UndecidableInstances       #-}

module Data.Locations.LocationAccessor where

import           Control.Lens                 ((^.))
-- import           Control.Funflow.ContentHashable
import           Control.Monad.Catch
import           Control.Monad.IO.Unlift
import           Control.Monad.ReaderSoup
import           Control.Monad.ReaderSoup.Resource ()
import           Control.Monad.Trans.Resource
import           Data.Aeson
import qualified Data.ByteString.Streaming    as BSS
import           Data.Locations.Loc
import qualified Data.Locations.LocationMonad as LM
-- import           Data.Store                      (Store)
import           GHC.TypeLits
import           Network.AWS
import qualified System.FilePath                       as Path
import qualified System.IO.Temp                        as Tmp


-- | Creates some Loc type, indexed over a symbol (see ReaderSoup for how that
-- symbol should be used), and equipped with functions to access it in some
-- Monad
class ( MonadMask m, MonadIO m
      , FromJSON (LocOf l), ToJSON (LocOf l) )
   => LocationAccessor (l::Symbol) m where

  data LocOf l :: *

  locExists :: LocOf l -> m Bool

  writeBSS :: LocOf l -> BSS.ByteString m r -> m r

  readBSS :: LocOf l -> (BSS.ByteString m () -> m b) -> m (Either LM.Error b)

  copy :: LocOf l -> LocOf l -> m (Either LM.Error ())
  copy locFrom locTo = readBSS locFrom (writeBSS locTo)

  withLocalBuffer :: (FilePath -> m a) -> LocOf l -> m a
  -- If we have a local resource accessor, we use it:
  default withLocalBuffer :: (MonadResource m)
                          => (FilePath -> m a) -> LocOf l -> m a
  withLocalBuffer f loc =
    Tmp.withSystemTempDirectory "pipeline-tools-tmp" writeAndUpload
    where
      writeAndUpload tmpDir = do
        let tmpFile = tmpDir Path.</> "out"
        res <- f tmpFile
        readBSS (L (localFile tmpFile)) (writeBSS loc)
        return res

-- | Accessing local resources
instance (MonadResource m, MonadMask m) => LocationAccessor "resource" m where
  newtype LocOf "resource" = L Loc
    deriving (FromJSON, ToJSON)
  locExists (L l) = LM.checkLocal "locExists" LM.locExists_Local l
  writeBSS (L l) = LM.checkLocal "writeBSS" LM.writeBSS_Local l
  readBSS (L l) f =
    LM.checkLocal "readBSS" (\l' -> LM.readBSS_Local l' f {->>= LM.eitherToExn-}) l
  withLocalBuffer f (L l) =
    LM.checkLocal "withLocalBuffer" (\l' -> f $ l'^.locFilePathAsRawFilePath) l
  copy (L l1) (L l2) =
    LM.checkLocal "copy" (\file1 ->
      LM.checkLocal "copy (2nd argument)" (LM.copy_Local file1) l2) l1
    -- >>= LM.eitherToExn

-- TODO: Move "aws" instance to its own porcupine-s3 package

-- | Accessing resources on S3
instance (MonadAWS m, MonadMask m, MonadResource m) => LocationAccessor "aws" m where
  newtype LocOf "aws" = S Loc
    deriving (FromJSON, ToJSON)
  locExists _ = return True -- TODO: Implement it
  writeBSS (S l) = LM.writeBSS_S3 l
  readBSS (S l) f = LM.readBSS_S3 l f -- >>= LM.eitherToExn
  copy (S l1) (S l2) = LM.copy_S3 l1 l2 -- >>= LM.eitherToExn

-- Temporary:
instance (IsInSoup ctxs "resource") => LM.LocationMonad (ReaderSoup ctxs) where
  locExists = locExists . L
  writeBSS = writeBSS . L
  readBSS = readBSS . L
  copy l1 l2 = copy (L l1) (L l2)
