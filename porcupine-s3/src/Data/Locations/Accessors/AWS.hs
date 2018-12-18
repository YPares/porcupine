{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE PatternSynonyms            #-}
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

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.ReaderSoup
import           Control.Monad.ReaderSoup.AWS
import           Control.Monad.ReaderSoup.Katip   ()
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.ByteString.Streaming        as BSS
import           Data.Locations.Accessors
import           Data.Locations.Loc
import           Data.Locations.LocationMonad     as LM
import           Data.String
import           Network.AWS                      hiding (Error)
import           Network.AWS.S3
import qualified Network.AWS.S3.TaskPipelineUtils as S3
import           System.TaskPipeline.Logger


-- | Just a compatiblity overlay for code explicitly dealing with S3 URLs
pattern S3Obj :: String -> LocFilePath a -> URLLikeLoc a
pattern S3Obj{bucketName,objectName} = RemoteFile "s3" bucketName objectName

-- | Accessing resources on S3
instance (MonadAWS m, MonadMask m, MonadResource m) => LocationAccessor m "aws" where
  newtype LocOf "aws" = S Loc
    deriving (ToJSON)
  locExists _ = return True -- TODO: Implement it
  writeBSS (S l) = writeBSS_S3 l
  readBSS (S l) f = readBSS_S3 l f -- >>= LM.eitherToExn
  copy (S l1) (S l2) = copy_S3 l1 l2 -- >>= LM.eitherToExn

instance (MonadAWS m, MonadMask m, MonadResource m) => MayProvideLocationAccessors m "aws"

instance FromJSON (LocOf "aws") where
  parseJSON v = do
    loc <- parseJSON v
    case loc of
      S3Obj{} -> return $ S loc
      _       -> fail "Doesn't use 's3' protocol"


writeBSS_S3 :: MonadAWS m => Loc -> BSS.ByteString m a -> m a
writeBSS_S3 S3Obj { bucketName, objectName } body = do
  let raw = objectName ^. locFilePathAsRawFilePath
  (res, r) <- S3.uploadObj (fromString bucketName) (fromString raw) body
  case res ^. porsResponseStatus of
    200 -> pure ()
    _   -> error $ "Unable to upload to the object " ++ raw ++ "."
  return r
writeBSS_S3 _ _ = undefined

readBSS_S3
  :: (MonadAWS m)
  => Loc
  -> (BSS.ByteString m () -> m b)
  -> m (Either Error b)
readBSS_S3 obj@S3Obj{ bucketName, objectName } k =
  mapLeft (Error obj . OtherError . displayException) <$> S3.streamObjInto
        (fromString bucketName)
        (fromString $ objectName ^. locFilePathAsRawFilePath)
        k
readBSS_S3 _ _ = undefined

copy_S3
  :: (MonadResource m, MonadAWS m)
  => Loc
  -> Loc
  -> m (Either Error ())
copy_S3 locFrom@(S3Obj bucket1 obj1) locTo@(S3Obj bucket2 obj2)
  | bucket1 == bucket2 = do
    _ <- S3.copyObj
          (fromString bucket1)
          (fromString $ obj1^.locFilePathAsRawFilePath)
          (fromString $ obj2^.locFilePathAsRawFilePath)
    pure (Right ())
  | otherwise = readBSS_S3 locFrom (writeBSS_S3 locTo)
copy_S3 _ _ = undefined


-- * Automatically switching from Resource to AWS monad, depending on some
-- reference Loc.

-- | Run a computation or a sequence of computations that will access some
-- locations. Selects whether to run in IO or AWS based on some Loc used as
-- selector.
selectRun :: URLLikeLoc t  -- ^ A Loc to use as switch (RunContext root or file)
          -> Bool -- ^ Verbosity
          -> (forall m. (LocationMonad m, MonadIO m, MonadBaseControl IO m) => m a)
             -- ^ The action to run, either in AWS or IO
          -> IO a
selectRun refLoc _verbose act =
  case refLoc of
    LocalFile{} ->
      runPorcupineM (baseContexts "selectRun_Local") act
    S3Obj{} ->
      runPorcupineM (#aws <-- useAWS Discover :& baseContexts "selectRun_AWS") act
    _ -> error "selectRun only handles local and S3 locations"

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
