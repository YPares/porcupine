{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC "-fno-warn-orphans" #-}
{-# OPTIONS_GHC "-fno-warn-name-shadowing" #-}

module Data.Locations.Accessors.AWS
  ( module Control.Monad.ReaderSoup.AWS
  , runPipelineTaskS3
  -- * Backward-compat API
  , selectRun
  , runWriteLazyByte
  , runReadLazyByte
  , runReadLazyByte_
  ) where

import           Control.Exception.Safe
import           Control.Lens
import           Control.Monad.ReaderSoup
import           Control.Monad.ReaderSoup.AWS
import           Control.Monad.ReaderSoup.Katip   ()
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.ByteString.Streaming        as BSS
import           Data.Locations.Accessors
import           Data.Locations.Loc
import           Data.String
import           Network.AWS                      hiding (Error)
import           Network.AWS.S3
import qualified Network.AWS.S3.TaskPipelineUtils as S3
import           System.TaskPipeline.CLI
import           System.TaskPipeline.PTask
import           System.TaskPipeline.Run


-- | Just a compatiblity overlay for code explicitly dealing with S3 URLs
pattern S3Obj :: String -> LocFilePath a -> URLLikeLoc a
pattern S3Obj{bucketName,objectName} = RemoteFile "s3" bucketName Nothing objectName

-- | Accessing resources on S3
instance (MonadAWS m, MonadMask m, MonadResource m)
      => LocationAccessor m "aws" where
  newtype GLocOf "aws" a = S (URLLikeLoc a)
    deriving (Functor, Foldable, Traversable, ToJSON, TypedLocation)
  locExists _ = return True -- TODO: Implement it
  writeBSS (S l) = writeBSS_S3 l
  readBSS (S l) f = readBSS_S3 l f
  copy (S l1) (S l2) = copy_S3 l1 l2

instance (MonadAWS m, MonadMask m, MonadResource m)
      => MayProvideLocationAccessors m "aws"

instance (IsLocString a) => Show (GLocOf "aws" a) where
  show (S l) = show l

instance (IsLocString a) => FromJSON (GLocOf "aws" a) where
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
  -> m b
readBSS_S3 S3Obj{ bucketName, objectName } k = do
  r <- S3.streamObjInto
         (fromString bucketName)
         (fromString $ objectName ^. locFilePathAsRawFilePath)
         k
  case r of
    Left e  -> throw e
    Right r -> return r
readBSS_S3 _ _ = undefined

copy_S3
  :: (MonadResource m, MonadAWS m)
  => Loc
  -> Loc
  -> m ()
copy_S3 locFrom@(S3Obj bucket1 obj1) locTo@(S3Obj bucket2 obj2)
  | bucket1 == bucket2 = do
      _ <- S3.copyObj
             (fromString bucket1)
             (fromString $ obj1^.locFilePathAsRawFilePath)
             (fromString $ obj2^.locFilePathAsRawFilePath)
      return ()
  | otherwise = readBSS_S3 locFrom (writeBSS_S3 locTo)
copy_S3 _ _ = undefined

-- | Just a shortcut for when you want ONLY local files and S3 support, with AWS
-- credentials discovery. Use 'runPipelineTask' if you want to activate other
-- location accessors.
runPipelineTaskS3
  :: PipelineConfigMethod o  -- ^ How to configure the pipeline
  -> Maybe Region            -- ^ Change the default AWS region
  -> PTask (ReaderSoup (("aws":::ContextFromName "aws") : BasePorcupineContexts)) () o
  -> IO o
runPipelineTaskS3 pcm mbRegion ptask =
  runPipelineTask pcm (  #aws <-- case mbRegion of
                                    Nothing  -> useAWS Discover
                                    Just reg -> useAWSRegion Discover reg
                      :& baseContexts (pcm ^. pipelineConfigMethodProgName) ) ptask ()


-- DEPRECATED CODE:

-- * Automatically switching from Resource to AWS monad when accessing some loc

-- | Run a computation or a sequence of computations that will access some
-- locations. Selects whether to run in IO or AWS based on some Loc used as
-- selector.
selectRun :: Loc  -- ^ A Loc to access
          -> (forall m l. (LocationAccessor m l) => LocOf l -> m a)
             -- ^ The action to run, either in AWS or IO
          -> IO a
selectRun loc f =
  case loc of
    LocalFile{} -> do
      let accessorsRec = baseContexts "selectRun_Local"
          (_,argsRec) = splitAccessorsFromArgRec accessorsRec
      consumeSoup argsRec $ f (L loc)
    S3Obj{} -> do
      let accessorsRec =    #aws <-- useAWS Discover
                         :& baseContexts "selectRun_AWS"
          (_,argsRec) = splitAccessorsFromArgRec accessorsRec
      consumeSoup argsRec $ f (S loc)
    _ -> error "selectRun only handles local and S3 locations"

-- | Just a shortcut
runWriteLazyByte
  :: Loc
  -> LBS.ByteString
  -> IO ()
runWriteLazyByte l bs = selectRun l $ \l' -> writeLazyByte l' bs

-- | Just a shortcut
runReadLazyByte, runReadLazyByte_ :: Loc -> IO LBS.ByteString
runReadLazyByte l = selectRun l readLazyByte
runReadLazyByte_ = runReadLazyByte
