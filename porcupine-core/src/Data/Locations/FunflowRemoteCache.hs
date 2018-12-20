{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Data.Locations.FunflowRemoteCache
  ( locationCacher
  , LiftCacher(..)
  ) where

import           Control.Exception.Safe
import           Control.Funflow.ContentHashable (ContentHash, hashToPath)
import qualified Control.Funflow.RemoteCache     as Remote
import           Control.Lens
import           Control.Monad.Trans
import           Data.Bifunctor                  (first)
import           Data.Locations.Loc
import           Data.Locations.LocationMonad
import           Katip
import           Path                            (toFilePath)
import           System.FilePath                 (dropTrailingPathSeparator)


hashToFilePath :: ContentHash -> FilePath
hashToFilePath = dropTrailingPathSeparator . toFilePath . hashToPath

newtype LocationCacher = LocationCacher Loc

tryS :: (MonadCatch m) => m a -> m (Either String a)
tryS = fmap (over _Left (displayException :: SomeException -> String)) . try

instance (LocationMonad m, KatipContext m)
      => Remote.Cacher m LocationCacher where
  push (LocationCacher loc) = Remote.pushAsArchive aliasPath $ \hash body -> do
    logFM DebugS $ logStr $
      "Remote cacher: Writing to file " ++ show (loc </> hashToFilePath hash)
    writeLazyByte (loc </> hashToFilePath hash) body
    pure Remote.PushOK
    where
      aliasPath from_ to_ = first show <$> tryS
        (copy
          (loc </> hashToFilePath from_)
          (loc </> hashToFilePath to_))
  pull (LocationCacher loc) = Remote.pullAsArchive $ \hash -> do
    logFM DebugS $ logStr $
      "Remote cacher: Reading from file " ++ show (loc </> hashToFilePath hash)
    readResult <- tryS $ readLazyByte (loc </> hashToFilePath hash)
    pure $ case readResult of
      Right bs -> Remote.PullOK bs
      Left err -> Remote.PullError err

locationCacher :: Maybe Loc -> Maybe LocationCacher
locationCacher = fmap LocationCacher

newtype LiftCacher cacher = LiftCacher cacher

instance (MonadTrans t, Remote.Cacher m cacher, Monad (t m)) =>
         Remote.Cacher (t m) (LiftCacher cacher) where
  push (LiftCacher c) hash hash2 path = lift $ Remote.push c hash hash2 path
  pull (LiftCacher c) hash path = lift $ Remote.pull c hash path
