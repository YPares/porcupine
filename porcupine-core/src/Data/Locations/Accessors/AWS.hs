{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC "-fno-warn-orphans" #-}

module Data.Locations.Accessors.AWS
  ( module Control.Monad.ReaderSoup.AWS
  , selectRun
  , runWriteLazyByte
  , runReadLazyByte
  , runReadLazyByte_
  ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.ReaderSoup
import           Control.Monad.ReaderSoup.AWS
import           Control.Monad.ReaderSoup.Katip ()
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Lazy           as LBS
import           Data.Locations.Accessors
import           Data.Locations.Loc
import           Data.Locations.LocationMonad   as LM
import           Network.AWS                    hiding (Error)
import           System.TaskPipeline.Logger


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
      _       -> fail "Is a local file"


-- * Automatically switching from Resource to AWS monad, depending on some
-- reference Loc

-- | Run a computation or a sequence of computations that will access some
-- locations. Selects whether to run in IO or AWS based on some Loc used as
-- selector.
--
-- You may want to use 'System.RunContext.runWithContext' which infers the Loc
-- switch and the verbosity level from the given context
selectRun :: Loc_ t  -- ^ A Loc to use as switch (RunContext root or file)
          -> Bool -- ^ Verbosity
          -> (forall m. (LocationMonad m, MonadIO m, MonadBaseControl IO m) => m a)
             -- ^ The action to run, either in AWS or IO
          -> IO a
selectRun refLoc _verbose act =
  case refLoc of
    LocalFile{} -> runPorcupineM (baseRec ()) act
    S3Obj{}     -> runPorcupineM (#aws <-- useAWS Discover :& baseRec ()) act
  where
           -- The unused arg is to prevent a too monomorphic type
    baseRec () = #katip    <-- ContextRunner (runLogger "" defaultLoggerScribeParams)
              :& #resource <-- useResource
              :& RNil

-- | Just a shortcut
runWriteLazyByte
  :: Loc
  -> LBS.ByteString
  -> IO ()
runWriteLazyByte l bs = selectRun l True $ writeLazyByte l bs

-- | Just a shortcut
runReadLazyByte :: Loc -> IO (Either Error LBS.ByteString)
runReadLazyByte l = selectRun l True $ readLazyByte l

-- | Just a shortcut
runReadLazyByte_ :: Loc -> IO LBS.ByteString
runReadLazyByte_ l = selectRun l True $ readLazyByte_ l
