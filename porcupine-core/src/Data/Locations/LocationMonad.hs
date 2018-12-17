{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Data.Locations.LocationMonad where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.ByteString.Streaming        as BSS
import           Data.Locations.Loc
import           Data.String
import qualified Data.Text                        as Text
import qualified Data.Text.Encoding               as TE
import           GHC.Generics                     (Generic)
import           Network.AWS                      hiding (Error)
import qualified Network.AWS                      as AWS
import           Network.AWS.S3
import qualified Network.AWS.S3.TaskPipelineUtils as S3
import           System.Directory                 (createDirectoryIfMissing,
                                                   createFileLink,
                                                   doesPathExist)
import qualified System.FilePath                  as Path
import qualified System.IO.Temp                   as Tmp


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

  writeBSS :: Loc -> BSS.ByteString m r -> m r

  -- | Read a (streaming) bytestring from a location.
  --
  -- For resources management reasons, the function is in CPS so that the
  -- underlying function can safely close the handle once the bytestring is
  -- consumed
  readBSS :: Loc -> (BSS.ByteString m () -> m b) -> m (Either Error b)

  -- |
  -- Duplicate a location.
  --
  -- As much as possible this should be done by aliasing to avoid actually
  -- moving the content around.
  copy ::
       Loc -- ^ From
    -> Loc -- ^ To
    -> m (Either Error ())
  copy = defaultCopy

defaultCopy :: LocationMonad m => Loc -> Loc -> m (Either Error ())
defaultCopy locFrom locTo = readBSS locFrom (writeBSS locTo)


-- | Wrapper to use functions directly writing to a filepath.
-- @withLocalBuffer f loc@ will apply @f@ to a temporary file and copy the
-- result to @loc@
withLocalBuffer :: (MonadIO m, MonadMask m, LocationMonad m) => (FilePath -> m a) -> Loc -> m a
withLocalBuffer f (LocalFile lf) = f $ lf ^. locFilePathAsRawFilePath
withLocalBuffer f loc =
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

-- | Just a compatiblity overlay for code explicitly dealing with S3 URLs
pattern S3Obj :: String -> LocFilePath a -> URLLikeLoc a
pattern S3Obj{bucketName,objectName} = RemoteFile "s3" bucketName objectName

writeBSS_S3 :: MonadAWS m => Loc -> BSS.ByteString m a -> m a
writeBSS_S3 S3Obj { bucketName, objectName } body = do
  let raw = objectName ^. locFilePathAsRawFilePath
  (res, r) <- S3.uploadObj (fromString bucketName) (fromString raw) body
  case res ^. porsResponseStatus of
    200 -> pure ()
    _   -> error $ "Unable to upload to the object " ++ raw ++ "."
  return r
writeBSS_S3 _ _ = undefined

writeLazyByte
  :: LocationMonad m
  => Loc
  -> LBS.ByteString
  -> m ()
writeLazyByte loc = writeBSS loc . BSS.fromLazy

eitherToExn :: (MonadThrow m, Exception e) => Either e a -> m a
eitherToExn (Left e)  = throwM e
eitherToExn (Right x) = pure x

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x)  = Left $ f x
mapLeft _ (Right y) = Right y

copy_Local :: MonadIO m => LocalFilePath -> LocalFilePath -> m (Either Error ())
copy_Local fp1 fp2 =
  liftIO $ Right <$> createFileLink
    (fp1^.locFilePathAsRawFilePath)
    (fp2^.locFilePathAsRawFilePath)

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

readBSS_Local
  :: forall f m a. (MonadCatch f, MonadResource m)
  => LocalFilePath
  -> (BSS.ByteString m () -> f a)
  -> f (Either Error a)
readBSS_Local f k = mapLeft (Error (LocalFile f) . IOError) <$>
  try (k $ BSS.readFile $ f ^. locFilePathAsRawFilePath)

readBSS_S3
  :: (MonadAWS m)
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
