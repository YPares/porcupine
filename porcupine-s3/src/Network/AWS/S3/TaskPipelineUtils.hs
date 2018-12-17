{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module Network.AWS.S3.TaskPipelineUtils
  ( runAll
  , getEnv
  , uploadObj
  , uploadFolder
  , streamS3Folder
  , streamObjInto
  , streamObjIntoExt
  , downloadFolder
  , copyObj
  )
where

import           Control.Lens                 hiding ((:>))
import           Control.Monad                (when)
import           Control.Monad.Catch          (catch, try)
import           Control.Monad.Trans.Resource
import           Control.Retry                (RetryPolicyM (..), limitRetries,
                                               retrying, rsIterNumber)
import qualified Data.ByteString.Streaming    as BSS
import           Data.Conduit.Binary          (sinkLbs)
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Network.AWS
import           Network.AWS                  (MonadAWS, liftAWS, send)
import           Network.AWS.Auth             (AuthError)
import           Network.AWS.Env              (Env, HasEnv, environment)
import           Network.AWS.S3
import           Network.AWS.S3               (BucketName, ObjectKey (..), oKey)
import qualified Network.AWS.S3.ListObjects   as LO
import qualified Streaming.Prelude            as S
import           Streaming.TaskPipelineUtils  as S
import           System.Directory             (createDirectoryIfMissing)
import           System.FilePath              (normalise, takeDirectory, (</>))


runAll :: AWS b -> IO b
runAll f = do
  env <- getEnv True
  runResourceT $ runAWS env f

-- These instances may overlap in theory, but in practice there is probably no
-- good reason to have two AWS.Envs in the same program, so only one side
-- should have one
instance HasEnv a => HasEnv (a `With` b) where environment = elt.environment
instance {-# OVERLAPPING #-} HasEnv (a `With` Env)
  where environment = ann.environment

getEnv :: Bool -- ^ Verbose
       -> IO Env
getEnv verbose = do
  -- Reads env vars AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY
  env <-
    catch (newEnv Discover) (handleException :: AuthError -> IO Env)
  pure $ env & envRegion .~ Frankfurt
  where
    handleException e = do
      when verbose $ do
        let err = show e
        putStrLn $ "Warning: couldn't get aws credentials, got " ++ err
        putStrLn $ "Dummy credentials will be used instead, " ++
                  "so anything trying to access S3 " ++
                  "or any other amazon service will probably fail"
      newEnv (FromKeys "foo" "bar")

uploadObj :: (MonadAWS m)
             => BucketName
             -> ObjectKey
             -> BSS.ByteString m a
             -> m (PutObjectResponse, a)
uploadObj buck object source = do
  (bs :> r) <- BSS.toStrict source
  por <- send $ putObject buck object $ toBody bs
  return (por, r)

copyObj ::
     (MonadAWS m)
  => BucketName
  -> Text
  -> ObjectKey
  -> m CopyObjectResponse
copyObj buck objFrom objTo = send $ copyObject buck objFrom objTo

-- | Upload a whole folder to an s3 bucket
uploadFolder :: (MonadAWS m, MonadResource m)
                => FilePath -- ^ Local folder to copy
                -> BucketName -- ^ Bucket to copy to
                -> FilePath -- ^ Remote path to copy the content of the folder in
                -> m ()
uploadFolder srcFolder destBucket destPath =
  streamFolderRel srcFolder
  & S.mapM_ (\f -> do
                let
                  objectName = destPath </> f
                (crs,_) <- uploadObj destBucket (fromString objectName) $ BSS.readFile f
                liftIO $ putStrLn $
                  if view porsResponseStatus crs == 200
                  then objectName ++ " uploaded."
                  else objectName ++ " upload failed.")

streamS3Folder ::
     MonadAWS m => BucketName -> Maybe FilePath -> Stream (Of FilePath) m ()
streamS3Folder bucketName prefix = do
  let listCommand = LO.listObjects bucketName
                      & LO.loPrefix .~ ((fromString . normalise) <$> prefix)
  rs <- lift $ liftAWS $ send listCommand
  view LO.lorsContents rs
    & S.each
    & S.map (view oKey)
    & S.map (\(ObjectKey k) -> T.unpack k)

downloadFolder :: (MonadAWS m, MonadResource m)
                  => BucketName
                  -> Maybe FilePath -- ^ The folder to download
                  -> FilePath -- ^ The path in which to save the download
                  -> m ()
downloadFolder srcBuck srcPath dest =
  streamS3Folder
    srcBuck
    srcPath
  & S.mapM_ (\f -> do
                let outFile = dest </> f
                liftIO $ createDirectoryIfMissing True $ takeDirectory outFile
                streamObjIntoExt srcBuck (fromString f) $ BSS.writeFile outFile)

streamObjInto :: (MonadAWS m)
                 => BucketName
                 -> ObjectKey
                 -> (BSS.ByteString m () -> m b)
                 -> m (Either Error b)
streamObjInto srcBuck srcObj f = retry (_svcRetry s3) . try $ do
  let g = getObject srcBuck srcObj
  rs <- send g
  resultingBS <- view gorsBody rs `sinkBody` sinkLbs
  f (BSS.fromLazy resultingBS)

-- |
-- Retries the given action until it succeeds or the maximum attemps has been
-- reached.
--
-- Amazonka has an automatic retry mechanism, except for streaming transfers,
-- and 'getObject' is streamed (so it doesn't have it).
-- This means that we have to implement our own retry mechanism, which is
-- a gross copy-paste of amazonka's internal mechanism.
--
-- Reference:
--      https://github.com/brendanhay/amazonka/blob/248f7b2a7248222cc21cef6194cd1872ba99ac5d/amazonka/src/Network/AWS/Internal/HTTP.hs#L180-L189
retry :: MonadIO m => Retry -> m (Either e a) -> m (Either e a)
retry awsRetry action =
  let
    retryPolicy =
      let
        Exponential {..} = awsRetry
        delay (rsIterNumber -> n)
            | n >= 0 = Just $ truncate (grow n * 1000000)
            | otherwise = Nothing
        grow n = _retryBase * (fromIntegral _retryGrowth ^^ (n - 1))
      in
      limitRetries _retryAttempts <> RetryPolicyM (return . delay)
    shouldRetry _ result =
      case result of
        Right _ -> pure False
        Left _  -> pure True
  in
  retrying retryPolicy shouldRetry (const action)


streamObjIntoExt :: (MonadAWS m)
                     => BucketName
                     -> ObjectKey
                     -> (BSS.ByteString m () -> m b)
                     -> m b
streamObjIntoExt srcBuck srcObj f = do
  streamResult <- streamObjInto srcBuck srcObj f
  case streamResult of
    Right x -> do
      liftIO $ putStrLn $ show srcObj ++ " downloaded."
      pure x
    Left err -> do
      liftIO $ putStrLn $ show srcObj ++ " download failed: " ++ show err
      f mempty
