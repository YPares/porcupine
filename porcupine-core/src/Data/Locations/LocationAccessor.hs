{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Data.Locations.LocationAccessor where

import           Control.Lens                 ((^.))
-- import           Control.Funflow.ContentHashable
import           Control.Monad.Catch
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Resource
import           Data.Aeson
-- import qualified Data.ByteString.Lazy            as LBS
import qualified Data.ByteString.Streaming    as BSS
import           Data.Locations.Loc
import qualified Data.Locations.LocationMonad as LM
-- import           Data.Store                      (Store)
import           GHC.TypeLits


-- | Creates some Loc type, indexed over a symbol (see ReaderSoup for how that
-- symbol should be used), and equipped with functions to access it in some
-- Monad
class (MonadMask m, MonadIO m
      ,FromJSON (LocOf l))
   => LocationAccessor (l::Symbol) m where

  data LocOf l :: *

  locExists :: LocOf l -> m Bool

  writeBSS :: LocOf l -> BSS.ByteString m r -> m r

  readBSS :: LocOf l -> (BSS.ByteString m () -> m b) -> m b

  withLocalBuffer :: (FilePath -> m a) -> LocOf l -> m a

-- Accessing local resources:
instance (MonadResource m, MonadMask m) => LocationAccessor "resource" m where
  newtype LocOf "resource" = L Loc
    deriving (FromJSON)
  locExists (L l) = LM.checkLocal "locExists" LM.locExists_Local l
  writeBSS (L l) = LM.checkLocal "writeBSS" LM.writeBSS_Local l
  readBSS (L l) f = LM.checkLocal "readBSS" rd l
    where rd l' = do r <- LM.readBSS_Local l' f
                     case r of
                       Left err -> throwM err
                       Right x  -> return x
  withLocalBuffer f (L l) =
    LM.checkLocal "withLocalBuffer" (\lf -> f $ lf^.locFilePathAsRawFilePath) l
