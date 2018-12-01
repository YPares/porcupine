{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Locations.LocationMonad where

import           Control.Lens
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Resource          (MonadResource)
import           Control.Monad.Trans.Resource.Internal (ResourceT (..))
import qualified Data.ByteString.Lazy                  as LBS
import qualified Data.ByteString.Streaming             as BSS
import           Data.Locations.Loc
import           Data.String
import qualified Data.Text                             as Text
import qualified Data.Text.Encoding                    as TE
import           GHC.Generics                          (Generic)
import           Katip.Monadic
import           Network.AWS                           hiding (Error)
import qualified Network.AWS                           as AWS
import           Network.AWS.S3
import qualified Network.AWS.S3.TaskPipelineUtils      as S3
import           Streaming
import           System.Directory                      (createDirectoryIfMissing,
                                                        doesPathExist)
import qualified System.FilePath                       as Path
import qualified System.IO.Temp                        as Tmp


-- | Alias for the constraints needed to manipulate remote files
-- type LocationConstraint m r = (MonadAWS m
--                               , AWSConstraint r m
--                               , MonadMask m)

data RawError
  = AWSError AWS.Error
  | IOError IOError
  deriving (Generic, Show)

data Error = Error Loc RawError
  deriving (Generic)

instance Show Error where
  show (Error loc raw) =
    "Error when trying to access file " ++ show loc ++ ": got " ++ show raw

instance Exception Error

data RetrievingError
  = FileReadError Error
  | DecodingError Loc Text.Text

instance Exception RetrievingError

instance Show RetrievingError where
  show (FileReadError loc) = "Impossible to read file " <> show loc
  show (DecodingError loc msg) =
    "Error while decoding file " <> show loc <> ": " <> Text.unpack msg


-- | Runs computations accessing 'Loc's
class (MonadMask m, MonadIO m) => LocationMonad m where
  -- | Tells whether a Loc corresponds to a physical file
  locExists :: Loc -> m Bool

  writeBSS :: Loc -> BSS.ByteString m () -> m ()

  -- | Read a (streaming) bytestring from a location.
  --
  -- For resources management reasons, the function is in CPS so that the
  -- underlying function can safely close the handle once the bytestring is
  -- consumed
  readBSS :: Loc -> (BSS.ByteString m () -> m b) -> m (Either Error b)

  -- | Wrapper to use functions directly writing to a filepath.
  -- @withLocalBuffer f loc@ will apply @f@ to a temporary file and copy the
  -- result to @loc@
  withLocalBuffer :: (FilePath -> m a) -> Loc -> m a

-- | Any ReaderT of some LocationMonad is also a LocationMonad
instance (LocationMonad m) => LocationMonad (ReaderT r m) where
  locExists = lift . locExists
  writeBSS loc bs = do
    st <- ask
    lift $ writeBSS loc $ hoist (flip runReaderT st) bs
  readBSS loc f = do
    st <- ask
    lift $ readBSS loc $ flip runReaderT st . f . hoist lift
  withLocalBuffer f loc = do
    st <- ask
    lift $ withLocalBuffer (flip runReaderT st . f) loc

-- | Same than the previous instance, we just lift through the @KatipContextT@
-- constructor
instance (LocationMonad m) => LocationMonad (KatipContextT m) where
  locExists = lift . locExists
  writeBSS loc bs = KatipContextT $ do
    st <- ask
    lift $ writeBSS loc $ hoist (flip (runReaderT . unKatipContextT) st) bs
  readBSS loc f = KatipContextT $ do
    st <- ask
    lift $ readBSS loc $ flip (runReaderT . unKatipContextT) st . f . hoist lift
  withLocalBuffer f loc = KatipContextT $ do
    st <- ask
    lift $ withLocalBuffer (flip (runReaderT . unKatipContextT) st . f) loc

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
selectRun (LocalFile{}) _verbose act =
  runResourceT act
selectRun (S3Obj{}) verbose act = do
  putStrLn "Opening AWS connection"
  awsEnv <- S3.getEnv verbose
  runResourceT $ runAWS awsEnv act

-- These instances have been removed from resourcet in version 1.2.0
instance MonadBase IO (ResourceT IO) where
    liftBase = lift . liftBase
instance MonadBaseControl IO (ResourceT IO) where
     type StM (ResourceT IO) a = StM IO a
     liftBaseWith f = ResourceT $ \reader' ->
         liftBaseWith $ \runInBase ->
             f $ runInBase . (\(ResourceT r) -> r reader'  )
     restoreM = ResourceT . const . restoreM

instance LocationMonad AWS where
  locExists (LocalFile l) = locExists_Local l
  locExists _             = return True -- TODO: Implement it
  writeBSS (LocalFile l) = writeBSS_Local l
  writeBSS l             = writeBSS_S3 l
  readBSS (LocalFile l) = readBSS_Local l
  readBSS l             = readBSS_S3 l
  withLocalBuffer f (LocalFile lf) = f $ lf ^. locFilePathAsRawFilePath
  withLocalBuffer f loc@S3Obj{} =
    Tmp.withSystemTempDirectory "pipeline-tools-tmp" writeAndUpload
    where
      writeAndUpload tmpDir = do
        let tmpFile = tmpDir Path.</> "out"
        res <- f tmpFile
        readBSS_ (localFile tmpFile) (writeBSS loc)
        return res

-- | A location monad that also contains an environment
type LocationMonadReader r m =
  (LocationMonad m, MonadReader r m, MonadIO m)

-- Will be removed once we have pure models with initializers (as we won't need
-- encapsulated lists of init functions anymore)

-- | Can be instantiated to any 'LocationMonadReader'
newtype AnyLocationMR r a = AnyLocationMR { unAnyLocationMR :: forall m. (LocationMonadReader r m) => m a }

checkLocal :: String -> (LocalFilePath -> p) -> Loc -> p
checkLocal _ f (LocalFile fname) = f fname
checkLocal funcName _ _ = error $ funcName ++ ": S3 location cannot be reached in IO! Need to use AWS"

-- | The LocationMonad for programs needing only to access local files.
type LocalM = ResourceT IO

instance LocationMonad LocalM where
  locExists = checkLocal "locExists" locExists_Local
  writeBSS = checkLocal "writeBSS" writeBSS_Local
  readBSS  = checkLocal "readBSS" readBSS_Local
  withLocalBuffer f = checkLocal "withLocalBuffer" (\lf -> f $ lf^.locFilePathAsRawFilePath)

writeText :: LocationMonad m
          => Loc
          -> Text.Text
          -> m ()
writeText loc body =
  let bsBody = BSS.fromStrict $ TE.encodeUtf8 body in
  writeBSS loc bsBody

locExists_Local :: MonadIO m => LocalFilePath -> m Bool
locExists_Local = liftIO . doesPathExist . view locFilePathAsRawFilePath

writeBSS_Local :: MonadResource m => LocalFilePath -> BSS.ByteString m b -> m b
writeBSS_Local path body = do
  let raw = path ^. locFilePathAsRawFilePath
  liftIO $ createDirectoryIfMissing True (Path.takeDirectory raw)
  BSS.writeFile raw body

writeBSS_S3 :: (HasEnv r, MonadReader r m, MonadResource m, MonadAWS m) => Loc -> BSS.ByteString m () -> m ()
writeBSS_S3 S3Obj { bucketName, objectName } body = do
  let raw = objectName ^. locFilePathAsRawFilePath
  res <- S3.uploadObj (fromString bucketName) (fromString raw) body
  case res ^. porsResponseStatus of
    200 -> pure ()
    _   -> error $ "Unable to upload to the object " ++ raw ++ "."
writeBSS_S3 _ _ = undefined

writeLazyByte
  :: LocationMonad m
  => Loc
  -> LBS.ByteString
  -> m ()
writeLazyByte loc = writeBSS loc . BSS.fromLazy

-- | Just a shortcut
runWriteLazyByte
  :: Loc
  -> LBS.ByteString
  -> IO ()
runWriteLazyByte l bs = selectRun l True $ writeLazyByte l bs

eitherToExn :: (MonadThrow m, Exception e) => Either e a -> m a
eitherToExn (Left e)  = throwM e
eitherToExn (Right x) = pure x

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x)  = Left $ f x
mapLeft _ (Right y) = Right y

readBSS_Local
  :: forall f m a. (MonadCatch f, MonadResource m)
  => LocalFilePath
  -> (BSS.ByteString m () -> f a)
  -> f (Either Error a)
readBSS_Local f k = mapLeft (Error (LocalFile f) . IOError) <$>
  try (k $ BSS.readFile $ f ^. locFilePathAsRawFilePath)

readBSS_S3
  :: (HasEnv r, MonadReader r m, MonadResource m, MonadAWS m)
  => Loc
  -> (BSS.ByteString m () -> m b)
  -> m (Either Error b)
readBSS_S3 obj@S3Obj{ bucketName, objectName } k =
  mapLeft (Error obj . AWSError) <$> S3.streamObjInto
        (fromString bucketName)
        (fromString $ objectName ^. locFilePathAsRawFilePath)
        k

readBSS_S3 _ _ = undefined

-- | Exception version of 'readBSS'
readBSS_ :: LocationMonad m
         => Loc
         -> (BSS.ByteString m () -> m b)
         -> m b
readBSS_ loc k = eitherToExn =<< readBSS loc k

readLazyByte :: LocationMonad m
                => Loc
                -> m (Either Error LBS.ByteString)
readLazyByte loc = readBSS loc BSS.toLazy_

readLazyByte_ :: LocationMonad m
                 => Loc
                 -> m LBS.ByteString
readLazyByte_ loc = eitherToExn =<< readLazyByte loc

-- | Just a shortcut
runReadLazyByte :: Loc -> IO (Either Error LBS.ByteString)
runReadLazyByte l = selectRun l True $ readLazyByte l

-- | Just a shortcut
runReadLazyByte_ :: Loc -> IO LBS.ByteString
runReadLazyByte_ l = selectRun l True $ readLazyByte_ l

readText :: LocationMonad m
            => Loc
            -> m (Either Error Text.Text)
readText loc = do
  maybeBSContent <- readLazyByte loc
  pure $ (TE.decodeUtf8 . LBS.toStrict) <$> maybeBSContent

readText_ :: LocationMonad m
             => Loc
             -> m Text.Text
readText_ loc = eitherToExn =<< readText loc
