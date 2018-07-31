{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Data.Locations.LocationMonad where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource     (MonadResource, MonadUnliftIO,
                                                   ResourceT)
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.ByteString.Streaming        as BSS
import           Data.Locations.Loc
import           Data.String
import qualified Data.Text                        as Text
import qualified Data.Text.Encoding               as TE
import           Formatting
import           Formatting.Clock
import           GHC.Generics                     (Generic)
import           Network.AWS                      hiding (Error)
import qualified Network.AWS                      as AWS
import           Network.AWS.S3
import qualified Network.AWS.S3.TaskPipelineUtils as S3
import           Streaming
import           System.Clock
import           System.Directory                 (createDirectoryIfMissing)
import qualified System.FilePath                  as Path
import qualified System.IO.Temp                   as Tmp


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

-- | Runs computations accessing 'Loc's
class (MonadMask m, MonadUnliftIO m) => LocationMonad m where
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

  -- | Log a message
  logMsg :: String -> m ()
  -- Redundant since for now we impose a 'MonadIO' constraint anyways
  -- default logMsg :: MonadIO m => String -> m ()
  logMsg = liftIO . putStrLn

  -- | Time an access duration
  clockAccess :: m a -> m a
  -- default clockAccess :: MonadIO m => m a -> m a
  clockAccess act = do
    start <- liftIO $ getTime Monotonic
    res   <- act
    end   <- liftIO $ getTime Monotonic
    liftIO $ fprint (timeSpecs % "\n") start end
    return res

-- | Any ReaderT of some LocationMonad is also a LocationMonad
instance (LocationMonad m) => LocationMonad (ReaderT r m) where
  writeBSS loc bs = do
    st <- ask
    lift $ writeBSS loc $ hoist (flip runReaderT st) bs
  readBSS loc f = do
    st <- ask
    lift $ readBSS loc $ flip runReaderT st . f . hoist lift
  withLocalBuffer f loc = do
    st <- ask
    lift $ withLocalBuffer (flip runReaderT st . f) loc
  logMsg = lift . logMsg
  clockAccess (ReaderT act) = ReaderT (clockAccess . act)

-- | Run a computation or a sequence of computations that will access some
-- locations. Selects whether to run in IO or AWS based on some Loc used as
-- selector.
--
-- You may want to use 'System.RunContext.runWithContext' which infers the Loc
-- switch and the verbosity level from the given context
selectRun :: Loc  -- ^ A Loc to use as switch (RunContext root or file)
          -> Bool -- ^ Verbosity
          -> (forall m. (LocationMonad m, MonadIO m) => m a)
             -- ^ The action to run, either in AWS or IO
          -> IO a
selectRun (LocalFile{}) _verbose act =
  runResourceT act
selectRun (S3Obj{}) verbose act = do
  putStrLn "Opening AWS connection"
  awsEnv <- S3.getEnv verbose
  runResourceT $ runAWS awsEnv act

instance LocationMonad AWS where
  writeBSS (LocalFile l) = writeBSS_Local l
  writeBSS l             = writeBSS_S3 l
  readBSS (LocalFile l) = readBSS_Local l
  readBSS l             = readBSS_S3 l
  withLocalBuffer f (LocalFile lf) = f lf
  withLocalBuffer f loc@S3Obj{} =
    Tmp.withSystemTempDirectory "simwork.tmp" writeAndUpload
    where
      writeAndUpload tmpDir = do
        let tmpFile = tmpDir Path.</> "out"
        res <- f tmpFile
        readBSS_ (LocalFile tmpFile) (writeBSS loc)
        return res

-- | A location monad that also contains an environment
type LocationMonadReader r m =
  (LocationMonad m, MonadReader r m, MonadIO m)

-- Will be removed once we have pure models with initializers (as we won't need
-- encapsulated lists of init functions anymore)

-- | Can be instantiated to any 'LocationMonadReader'
newtype AnyLocationMR r a = AnyLocationMR { unAnyLocationMR :: forall m. (LocationMonadReader r m) => m a }

checkLocal :: [Char] -> (FilePath -> p) -> Loc -> p
checkLocal _ f (LocalFile fname) = f fname
checkLocal funcName _ _ = error $ funcName ++ ": S3 location cannot be reached in IO! Need to use AWS"

-- | The LocationMonad for programs needing only to access local files.
type LocalM = ResourceT IO

instance LocationMonad LocalM where
  writeBSS = checkLocal "writeBSS" writeBSS_Local
  readBSS  = checkLocal "readBSS" readBSS_Local
  withLocalBuffer f = checkLocal "withLocalBuffer" (\lf -> f lf)

writeText :: LocationMonad m
             => Loc
             -> Text.Text
             -> m ()
writeText loc body =
  let bsBody = BSS.fromStrict $ TE.encodeUtf8 body in
  writeBSS loc bsBody

writeBSS_Local :: MonadResource m => FilePath -> BSS.ByteString m b -> m b
writeBSS_Local path body = do
  liftIO $ createDirectoryIfMissing True (Path.takeDirectory path)
  BSS.writeFile path body

writeBSS_S3 :: (HasEnv r, MonadReader r m, MonadResource m, MonadAWS m) => Loc -> BSS.ByteString m () -> m ()
writeBSS_S3 S3Obj { bucketName, objectName } body = do
  res <- S3.uploadObj (fromString bucketName) (fromString objectName) body
  case view porsResponseStatus res of
    200 -> pure ()
    _   -> error $ "Unable to upload to the object " ++ objectName ++ "."
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
  => FilePath
  -> (BSS.ByteString m () -> f a)
  -> f (Either Error a)
readBSS_Local f k = mapLeft (Error (LocalFile f) . IOError) <$>
  try (k $ BSS.readFile f)

readBSS_S3 :: (HasEnv r, MonadReader r m, MonadResource m, MonadAWS m) => Loc -> (BSS.ByteString m () -> m b) -> m (Either Error b)
readBSS_S3 obj@S3Obj{ bucketName, objectName } k =
  mapLeft (Error obj . AWSError) <$> S3.streamObjInto
        (fromString bucketName)
        (fromString objectName)
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
