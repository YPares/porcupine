{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE StaticPointers       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Data.Locations.Loc where

import           Control.Applicative
import           Control.DeepSeq             (NFData)
import qualified Control.Distributed.Closure as C
import           Control.Lens
import           Data.Aeson
import           Data.Binary                 (Binary)
import           Data.Data                   (Data, Typeable)
import           Data.Maybe
import           Data.Representable
import           Data.String
import qualified Data.Text                   as T
import           GHC.Generics                (Generic)
import qualified Network.URL                 as URL
import qualified System.Directory            as Dir (createDirectoryIfMissing)
import qualified System.FilePath             as Path
import qualified System.FilePath.Lens        as PL

-- | Location's main type. A value of type @Loc@ denotes a file or a folder
-- that may be local or hosted remotely (s3).
data Loc
  = LocalFile { filePath :: FilePath }
  | S3Obj { bucketName :: String
          , objectName :: FilePath }
  {- In the future, we might want to add the following locations
     | ParquetObj ...
     | SQLTableObj ...
  -}
  deriving (Eq, Ord, Generic, Data, Typeable, ToJSON, FromJSON)

instance Binary Loc

instance C.Static (C.Serializable Loc) where
  closureDict = static C.Dict

instance NFData Loc

instance Show Loc where
  show LocalFile{ filePath } = filePath
  show S3Obj{ bucketName, objectName } =
    "s3://" ++ bucketName ++ "/" ++ objectName

-- | Lens to the path in the 'Loc'
locPath :: Lens' Loc FilePath
locPath f LocalFile{ filePath } = LocalFile <$> f filePath
locPath f S3Obj{ bucketName, objectName } =
  (\n' -> S3Obj { bucketName, objectName = n' }) <$> f objectName

-- | Lens to the extension of the 'Loc'
locExt :: Lens' Loc T.Text
locExt = locPath . PL.extension . iso T.pack T.unpack

parseURL :: String -> Maybe Loc
parseURL litteralPath = do
        pathUrl <- URL.importURL litteralPath
        pure $ case URL.url_type pathUrl of
          URL.Absolute h ->
            case URL.protocol h of
              URL.RawProt "s3" ->
                S3Obj { bucketName = URL.host h
                      , objectName = URL.url_path pathUrl }
              p -> error $ "Unsupported protocol: " ++ show p
          URL.HostRelative -> LocalFile "/" <//> URL.url_path pathUrl
          URL.PathRelative -> LocalFile $ URL.url_path pathUrl

instance IsString Loc where
  fromString = fromJust . parseURL

instance Representable Loc where
  toTextRepr = T.pack . show
  fromTextRepr x = case parseURL . T.unpack $ x of
    Nothing -> empty
    Just x' -> pure x'

-- | Appends a path to a location
infixl 4 </>
(</>) :: Loc -> FilePath -> Loc
f </> p = over locPath (Path.</> p) f

-- | Alias for '</>'
infixl 4 <//>
(<//>) :: Loc -> FilePath -> Loc
(<//>) = (</>)

infixl 3 <.>
(<.>) :: Loc -> String -> Loc
f <.> ext = over locPath (Path.<.> ext) f

infixl 3 -<.>
(-<.>) :: Loc -> FilePath -> Loc
f -<.> ext = over locPath (Path.-<.> ext) f

-- | Initialises a directory, so that we can safely write in it afterwards.
-- For a local filesystem, this means creating it
initDir :: Loc -> IO ()
initDir (LocalFile f) =
  Dir.createDirectoryIfMissing True f
initDir S3Obj{} = pure ()

-- | Open the selected folder in a default application
openFolder :: Loc -> IO ()
openFolder = print

-- | Analog to 'Path.takeDirectory' for generalized locations
takeDirectory :: Loc -> Loc
takeDirectory = over locPath Path.takeDirectory

-- | Analog of 'Path.dropExtension'
dropExtension :: Loc -> Loc
dropExtension = over locPath Path.dropExtension

-- -- | Upload a whole folder to an s3 bucket, respecting the hierarchy inside
-- -- this folder
-- --
-- -- Because I'm lazy, this will fail on local files
-- uploadFolder
--   :: FilePath
--   -> Loc
--   -> AWS ()
-- uploadFolder _src (LocalFile _f) =
--   error "uploadFolder not implemented for local files"
-- uploadFolder src (S3Obj buck remotePath) =
--   S3.uploadFolder src (fromString buck) remotePath
