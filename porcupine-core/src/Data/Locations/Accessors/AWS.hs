{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Data.Locations.Accessors.AWS
  ( module Control.Monad.ReaderSoup.AWS
  ) where

import           Control.Monad.Catch
import           Control.Monad.ReaderSoup.AWS
import           Control.Monad.Trans.Resource
import           Data.Locations.Accessors
import           Data.Locations.Loc
import qualified Data.Locations.LocationMonad as LM
import           Network.AWS                  (MonadAWS)


-- TODO: Move "aws" instance to its own porcupine-s3 package

-- | Accessing resources on S3
instance (MonadAWS m, MonadMask m, MonadResource m) => LocationAccessor m "aws" where
  newtype LocOf "aws" = S Loc
    deriving (ToJSON)
  locExists _ = return True -- TODO: Implement it
  writeBSS (S l) = LM.writeBSS_S3 l
  readBSS (S l) f = LM.readBSS_S3 l f -- >>= LM.eitherToExn
  copy (S l1) (S l2) = LM.copy_S3 l1 l2 -- >>= LM.eitherToExn

instance (MonadAWS m, MonadMask m, MonadResource m) => MayProvideLocationAccessors m "aws"

instance FromJSON (LocOf "aws") where
  parseJSON v = do
    loc <- parseJSON v
    case loc of
      S3Obj{} -> return $ S loc
      _ -> fail "Is a local file"
