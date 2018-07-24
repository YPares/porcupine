{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Network.AWS.S3.TaskPipelineUtils
  (
    runAll
  , getEnv
  , uploadObj
  , uploadFolder
  , streamObjInto
  , streamObjIntoExt
  , downloadFolder
  )
where

import           Control.Lens
import           Control.Monad                  (when)
import           Control.Monad.Catch            (catch, try)
import           Control.Monad.Trans.AWS
import qualified Data.ByteString.Streaming      as BSS
import           Data.String
import           Network.AWS                    hiding (send)
import           Network.AWS.Auth               (AuthError)
import           Network.AWS.S3
import           Network.AWS.S3.StreamingUpload
import           Streaming.TaskPipelineUtils    as S
import           System.Directory               (createDirectoryIfMissing)
import           System.FilePath                (takeDirectory, (</>))


runAll :: AWS b -> IO b
runAll f = do
  env <- getEnv True
  runResourceT $ runAWS env f

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

uploadObj :: (MonadAWS m, AWSConstraint r m)
             => BucketName
             -> ObjectKey
             -> BSS.ByteString m ()
             -> m CompleteMultipartUploadResponse
uploadObj buck object source = do
  let sink =
        streamUpload Nothing $
          -- createMultipartUpload (fromString (bucketName info)) (fromString (objName info))
          createMultipartUpload buck object
  intoSink sink $ BSS.toChunks source
  --return $ view crsResponseStatus ur

-- | Upload a whole folder to an s3 bucket
uploadFolder :: (MonadAWS m, AWSConstraint r m)
                => FilePath -- ^ Local folder to copy
                -> BucketName -- ^ Bucket to copy to
                -> FilePath -- ^ Remote path to copy the content of the folder in
                -> m ()
uploadFolder srcFolder destBucket destPath =
  streamFolderRel srcFolder
  & S.mapM_ (\f -> do
                let
                  objectName = destPath </> f
                crs <- uploadObj destBucket (fromString objectName) $ BSS.readFile f
                liftIO $ putStrLn $
                  if view crsResponseStatus crs == 200
                  then objectName ++ " uploaded."
                  else objectName ++ " upload failed.")

downloadFolder :: (MonadAWS m, AWSConstraint r m)
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

streamObjInto :: (MonadAWS m, AWSConstraint r m)
                 => BucketName
                 -> ObjectKey
                 -> (BSS.ByteString m () -> m b)
                 -> m (Either Error b)
streamObjInto srcBuck srcObj f = try $ do
  let g = getObject srcBuck srcObj
  rs <- send g
  view gorsBody rs `sinkBody` asConduit (lift . f . BSS.fromChunks)

streamObjIntoExt :: (MonadAWS m, AWSConstraint r m)
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
