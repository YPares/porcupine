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
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Data.Locations.LocationMonad where

import           Control.Exception.Safe
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource hiding (MonadThrow (..))
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.ByteString.Streaming    as BSS
import           Data.Locations.Loc
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as TE
import           System.Directory             (createDirectoryIfMissing,
                                               createFileLink, doesPathExist)
import qualified System.FilePath              as Path
import qualified System.IO.Temp               as Tmp


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
  readBSS :: Loc -> (BSS.ByteString m () -> m a) -> m a

  -- |
  -- Duplicate a location.
  --
  -- As much as possible this should be done by aliasing to avoid actually
  -- moving the content around.
  copy ::
       Loc -- ^ From
    -> Loc -- ^ To
    -> m ()
  copy = defaultCopy

defaultCopy :: LocationMonad m => Loc -> Loc -> m ()
defaultCopy locFrom locTo = readBSS locFrom (writeBSS locTo)


-- | Wrapper to use functions directly writing to a filepath.
-- @withLocalBuffer f loc@ will apply @f@ to a temporary file and copy the
-- result to @loc@
withLocalBuffer :: (MonadIO m, LocationMonad m) => (FilePath -> m a) -> Loc -> m a
withLocalBuffer f (LocalFile lf) = f $ lf ^. locFilePathAsRawFilePath
withLocalBuffer f loc =
    Tmp.withSystemTempDirectory "pipeline-tools-tmp" writeAndUpload
    where
      writeAndUpload tmpDir = do
        let tmpFile = tmpDir Path.</> "out"
        res <- f tmpFile
        readBSS (localFile tmpFile) (writeBSS loc)
        return res

-- Will be removed once we have pure models with initializers (as we won't need
-- encapsulated lists of init functions anymore)


checkLocal :: String -> (LocalFilePath -> p) -> Loc -> p
checkLocal _ f (LocalFile fname) = f fname
checkLocal funcName _ loc = error $ funcName ++ ": location " ++ show loc ++ " isn't a LocalFile"


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

eitherToExn :: (MonadThrow m, Exception e) => Either e a -> m a
eitherToExn (Left e)  = throwM e
eitherToExn (Right x) = pure x

copy_Local :: MonadIO m => LocalFilePath -> LocalFilePath -> m ()
copy_Local fp1 fp2 =
  liftIO $ createFileLink
    (fp1^.locFilePathAsRawFilePath)
    (fp2^.locFilePathAsRawFilePath)

readBSS_Local
  :: forall f m a. (MonadResource m)
  => LocalFilePath
  -> (BSS.ByteString m () -> f a)
  -> f a
readBSS_Local f k = k $ BSS.readFile $ f ^. locFilePathAsRawFilePath


writeLazyByte
  :: LocationMonad m
  => Loc
  -> LBS.ByteString
  -> m ()
writeLazyByte loc = writeBSS loc . BSS.fromLazy

-- The following functions are DEPRECATED, because converting to a lazy
-- ByteString with BSS.toLazy_ isn't actually lazy

readLazyByte :: LocationMonad m
                => Loc
                -> m LBS.ByteString
readLazyByte loc = readBSS loc BSS.toLazy_

readText :: LocationMonad m
            => Loc
            -> m Text.Text
readText loc =
  TE.decodeUtf8 . LBS.toStrict <$> readLazyByte loc
