{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE StaticPointers       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# OPTIONS_GHC -Wall #-}

module Data.Locations.Loc where

import           Control.Applicative
import           Control.DeepSeq             (NFData)
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
import qualified Data.HashMap.Strict as HM


-- | Each location can be a simple raw string or a string containing references
-- to variables, waiting to be spliced in.
data LocBit
  = LocBitString String  -- ^ A raw filepath part, to be used as is
  | LocBitVarRef String -- ^ A variable name
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- instance Show LocBit where
--   show (LocBitString s) = s
--   show (LocBitVarRef v) = "{" ++ v ++ "}"

-- | A newtype so that we can redefine the Show instance
newtype LocString = LocString [LocBit]

instance Show LocString where
  show (LocString l) = concatMap show l

-- | True if the loc is just one 'LocPartRawString'
isLocStringComplete :: LocString -> Bool
isLocStringComplete (LocString [LocBitString _]) = True
isLocStringComplete _ = False

-- | Location's main type. A value of type @Loc@ denotes a file or a folder
-- that may be local or hosted remotely (s3).
data Loc_ a
  = LocalFile { filePath :: a }
  | S3Obj { bucketName :: a
          , objectName :: a }
  {- In the future, we might want to add the following locations
     | ParquetObj ...
     | SQLTableObj ...
  -}
  deriving (Eq, Ord, Generic, ToJSON, FromJSON, Functor, NFData, Binary)

type Loc = Loc_ LocString

type ResolvedLoc = Loc_ String

instance (Show a) => Show (Loc_ a) where
  show LocalFile{ filePath } = show filePath
  show S3Obj{ bucketName, objectName } =
    "s3://" ++ show bucketName ++ "/" ++ show objectName

isLocComplete :: Loc -> Bool
isLocComplete = allOf traversed isLocStringComplete

-- | Lens to the path in the 'Loc'
locPath :: Lens' Loc FilePath
locPath f LocalFile{ filePath } = LocalFile <$> f filePath
locPath f S3Obj{ bucketName, objectName } =
  (\n' -> S3Obj{ bucketName, objectName = n' }) <$> f objectName

-- | Lens to the extension of the 'Loc'
locExt :: Lens' Loc T.Text
locExt = locPath . PL.extension . iso T.pack T.unpack

parseLocString :: String -> Either String LocString
parseLocString s = (LocString . reverse . map reverseLB . filter isFull)
                   <$> foldM oneChar [] s
  where
    oneChar (LocBitVarRef _ : _) '{' = Left "Cannot nest '{...}'"
    oneChar acc '{' = return $ LocBitVarRef "" : acc
    oneChar (LocBitString _ : _) '}' = Left "'}' terminates nothing"
    oneChar acc '}' = return $ LocBitString "" : acc
    oneChar (hd : rest) c = return $ add c hd : rest
    oneChar [] c = return [LocBitString [c]]

    add c (LocBitVarRef v) = LocBitVarRef $ c:v
    add c (LocBitString v) = LocBitString $ c:v

    isFull (LocBitString "") = False
    isFull _ = True

    reverseLB (LocBitVarRef v) = LocBitVarRef $ reverse v
    reverseLB (LocBitString v) = LocBitString $ reverse v

parseURL :: String -> Maybe Loc
parseURL litteralPath = do
        pathUrl <- URL.importURL litteralPath
        case URL.url_type pathUrl of
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

addExtToLocIfMissing :: Loc -> T.Text -> Loc
addExtToLocIfMissing loc ext | T.null (loc^.locExt) =
  if T.null ext
     then loc
     else loc & locExt .~ ext
addExtToLocIfMissing loc _ = loc
