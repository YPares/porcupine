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
import           Data.Locations.Accessors
import           Data.Locations.Loc
import           Katip
import           Path                            (toFilePath)
import           System.FilePath                 (dropTrailingPathSeparator)


hashToFilePath :: ContentHash -> FilePath
hashToFilePath = dropTrailingPathSeparator . toFilePath . hashToPath

newtype LocationCacher m = LocationCacher (SomeLoc m)

tryS :: (MonadCatch m) => m a -> m (Either String a)
tryS = fmap (over _Left (displayException :: SomeException -> String)) . try

instance (KatipContext m)
      => Remote.Cacher m (LocationCacher m) where
  push (LocationCacher (SomeGLoc rootLoc)) = Remote.pushAsArchive aliasPath $ \hash body -> do
    let loc = rootLoc `addSubdirToLoc` hashToFilePath hash
    katipAddNamespace "remoteCacher" $ logFM DebugS $ logStr $
      "Writing to file " ++ show loc
    writeLazyByte loc body
    pure Remote.PushOK
    where
      aliasPath from_ to_ = first show <$> tryS
        (copy
          (rootLoc `addSubdirToLoc` hashToFilePath from_)
          (rootLoc `addSubdirToLoc` hashToFilePath to_))
  pull (LocationCacher (SomeGLoc rootLoc)) = Remote.pullAsArchive $ \hash ->
    katipAddNamespace "remoteCacher" $ do
      let loc = rootLoc `addSubdirToLoc` hashToFilePath hash
      readResult <- tryS $ readLazyByte loc
      case readResult of
        Right bs -> do
          logFM DebugS $ logStr $ "Found in remote cache " ++ show loc
          return $ Remote.PullOK bs
        Left err -> do
          katipAddContext (sl "errorFromRemoteCache" err) $
            logFM DebugS $ logStr $ "Not in remote cache " ++ show loc
          return $ Remote.PullError err

locationCacher :: Maybe (SomeLoc m) -> Maybe (LocationCacher m)
locationCacher = fmap LocationCacher

newtype LiftCacher cacher = LiftCacher cacher

instance (MonadTrans t, Remote.Cacher m cacher, Monad (t m)) =>
         Remote.Cacher (t m) (LiftCacher cacher) where
  push (LiftCacher c) hash hash2 path = lift $ Remote.push c hash hash2 path
  pull (LiftCacher c) hash path = lift $ Remote.pull c hash path
