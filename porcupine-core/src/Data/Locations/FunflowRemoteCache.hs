{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Locations.FunflowRemoteCache
  ( locationCacher
  , LiftCacher(..)
  ) where

import           Control.Funflow.ContentHashable (ContentHash, hashToPath)
import qualified Control.Funflow.RemoteCache     as Remote
import           Control.Monad.Trans
import           Data.Locations.Loc
import           Data.Locations.LocationMonad
import           Path                            (toFilePath)
import System.FilePath (dropTrailingPathSeparator)

hashToFilePath :: ContentHash -> FilePath
hashToFilePath = dropTrailingPathSeparator . toFilePath . hashToPath

newtype LocationCacher = LocationCacher Loc

instance LocationMonad m => Remote.Cacher m LocationCacher where
  push (LocationCacher loc) = Remote.pushAsArchive aliasPath $ \hash body -> do
    logMsg $ "Writing to file " ++ show (loc </> hashToFilePath hash)
    writeLazyByte (loc </> hashToFilePath hash) body
    pure Remote.PushOK
    where
      aliasPath from to = Right <$>
        alias
          (loc </> hashToFilePath from)
          (loc </> hashToFilePath to)
  pull (LocationCacher loc) = Remote.pullAsArchive $ \hash -> do
    logMsg $ "Reading from file " ++ show (loc </> hashToFilePath hash)
    readResult <- readLazyByte (loc </> hashToFilePath hash)
    pure $ case readResult of
      Right bs -> Remote.PullOK bs
      Left err -> Remote.PullError (show err)

locationCacher :: Maybe Loc -> Maybe LocationCacher
locationCacher = fmap LocationCacher

newtype LiftCacher cacher = LiftCacher cacher

instance (MonadTrans t, Remote.Cacher m cacher, Monad (t m)) =>
         Remote.Cacher (t m) (LiftCacher cacher) where
  push (LiftCacher c) hash hash2 path = lift $ Remote.push c hash hash2 path
  pull (LiftCacher c) hash path = lift $ Remote.pull c hash path
