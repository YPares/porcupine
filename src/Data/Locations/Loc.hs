{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE StaticPointers       #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Data.Locations.Loc where

import           Control.Applicative
import           Control.Lens
import           Control.Monad       (foldM)
import           Data.Aeson
import           Data.Binary         (Binary)
import qualified Data.HashMap.Strict as HM
import           Data.Representable
import           Data.String
import qualified Data.Text           as T
import           GHC.Generics        (Generic)
import qualified Network.URL         as URL
import qualified System.Directory    as Dir (createDirectoryIfMissing)
import qualified System.FilePath     as Path


-- | Each location bit can be a simple chunk of string, or a variable name
-- waiting to be spliced in.
data LocBit
  = LocBitChunk FilePath  -- ^ A raw filepath part, to be used as is
  | LocBitVarRef String -- ^ A variable name
  deriving (Eq, Ord, Generic, ToJSON, FromJSON)

instance Show LocBit where
  show (LocBitChunk s)  = s
  show (LocBitVarRef v) = "{" ++ v ++ "}"

locBitContent :: Lens' LocBit String
locBitContent f (LocBitChunk p)  = LocBitChunk <$> f p
locBitContent f (LocBitVarRef v) = LocBitVarRef <$> f v

-- | A newtype so that we can redefine the Show instance
newtype LocString = LocString [LocBit]

instance Show LocString where
  show (LocString l) = concatMap show l

-- | Get all the variable names still in the loc string and possibly replace
-- them.
locStringVariables :: Traversal' LocString LocBit
locStringVariables f (LocString bits) = LocString . concatChunks <$> traverse f' bits
  where f' c@(LocBitChunk{})  = pure c
        f' c@(LocBitVarRef{}) = f c
        concatChunks (LocBitChunk p1 : LocBitChunk p2 : rest) =
          concatChunks (LocBitChunk (p1++p2) : rest)
        concatChunks (x : rest) = x : concatChunks rest
        concatChunks [] = []

data LocFilePath a = LocFilePath { _pathWithoutExt :: a, _pathExtension :: String }
  deriving (Eq, Ord, Generic, ToJSON, FromJSON, Functor, Foldable, Traversable, Binary)

makeLenses ''LocFilePath

-- | Turns the 'LocFilePath' to/from a simple string
rawFilePath :: Iso' (LocFilePath String) FilePath
rawFilePath = iso to_ from_
  where
    to_ (LocFilePath p e) = case e of
      "" -> p
      _  -> p++"."++e
    from_ fp = uncurry LocFilePath $ splitExtension' fp

instance (IsLocPath a) => Show (LocFilePath a) where
  show p = fmap showLocPath p ^. rawFilePath

-- | Location's main type. A value of type 'Loc_' denotes a file or a folder
-- that may be local or hosted remotely (s3).
data Loc_ a
  = LocalFile { filePath :: LocFilePath a }
  | S3Obj { bucketName :: String
          , objectName :: LocFilePath a }
  {- In the future, we might want to add the following locations
     | ParquetObj ...
     | SQLTableObj ...
  -}
  deriving (Eq, Ord, Generic, ToJSON, FromJSON, Functor, Foldable, Traversable, Binary)

instance (IsLocPath a) => Show (Loc_ a) where
  show LocalFile{ filePath } = show filePath
  show S3Obj{ bucketName, objectName } =
    "s3://" ++ bucketName ++ "/" ++ show objectName

-- | Lens to the 'LocFilePath' of the 'Loc'
locFilePath :: Lens (Loc_ a) (Loc_ b) (LocFilePath a) (LocFilePath b)
locFilePath f (LocalFile fp) = LocalFile <$> f fp
locFilePath f (S3Obj b fp)   = S3Obj b <$> f fp

-- | Lens to the extension of the 'Loc'
locExt :: Lens' (Loc_ a) String
locExt = locFilePath . pathExtension

-- | A 'Loc_' that might contain some names holes, called variables, that we
-- have first to replace by a value before we can get a definite physical
-- location.
type LocWithVars = Loc_ LocString

-- | A 'Loc_' that can directly be accessed as is.
type Loc = Loc_ String

-- | Creates a 'Loc' from a simple litteral string
localFile :: FilePath -> Loc
localFile s = LocalFile $ s ^. from rawFilePath

-- | Creates a 'LocWithVars' that will only contain a chunk, no variables
locWithVarsFromLoc :: Loc -> LocWithVars
locWithVarsFromLoc = fmap (LocString . (:[]) . LocBitChunk)

-- | All the variables leftover in a 'LocWithVars'
locVariables :: Traversal' LocWithVars LocBit
locVariables = traversed . locStringVariables

-- | Splices in the variables present in the hashmap
spliceLocVariables :: HM.HashMap String String -> LocWithVars -> LocWithVars
spliceLocVariables vars = over locVariables $ \v@(LocBitVarRef vname) ->
  case HM.lookup vname vars of
    Just val -> LocBitChunk val
    Nothing  -> v

-- | Can represent file paths
class IsLocPath a where
  appendBeforePath :: String -> a -> a
  appendAfterPath :: a -> String -> a
  showLocPath :: a -> String  -- We can't use show because it adds quotes when showing a String
  parseLocStringAndExt :: String -> Either String (LocFilePath a)

splitExtension' :: FilePath -> (FilePath, String)
splitExtension' fp = let (f,e) = Path.splitExtension fp in
  case e of '.':e' -> (f,e')
            _      -> (f,e)

instance IsLocPath FilePath where
  appendBeforePath = (Path.</>)
  appendAfterPath = (Path.</>)
  showLocPath = id
  parseLocStringAndExt fp = Right $ fp ^. from rawFilePath

parseLocString :: String -> Either String LocString
parseLocString s = (LocString . reverse . map (over locBitContent reverse) . filter isFull)
                   <$> foldM oneChar [] s
  where
    oneChar (LocBitVarRef _ : _) '{' = Left "Cannot nest {...}"
    oneChar acc '{' = return $ LocBitVarRef "" : acc
    oneChar (LocBitChunk _ : _) '}' = Left "'}' terminates nothing"
    oneChar acc '}' = return $ LocBitChunk "" : acc
    oneChar (hd : rest) c = return $ over locBitContent (c:) hd : rest
    oneChar [] c = return [LocBitChunk [c]]

    isFull (LocBitChunk "") = False
    isFull _                = True

refuseVarRefs :: String -> String -> Either String String
refuseVarRefs place s = do
  l <- parseLocString s
  case l of
    (LocString []) -> return ""
    (LocString [LocBitChunk p]) -> return p
    _ -> Left $ "Variable references {...} are not allowed in the " ++ place ++ " part of a URL"

instance IsLocPath LocString where
  appendBeforePath a (LocString b) = LocString $ case b of
    LocBitChunk c : rest -> LocBitChunk (a ++ "/" ++ c) : rest
    _                    -> LocBitChunk (a++"/") : b
  appendAfterPath (LocString a) b = LocString $ reverse $ case reverse a of
    LocBitChunk c : rest -> LocBitChunk (c ++ "/" ++ b) : rest
    a'                   -> LocBitChunk ('/':b) : a'
  showLocPath = show
  parseLocStringAndExt s =
    LocFilePath <$> parseLocString p <*> refuseVarRefs "extension" e
    where (p, e) = splitExtension' s

-- | The main way to parse a 'Loc_'.
parseURL :: (IsLocPath a) => String -> Either String (Loc_ a)
parseURL litteralPath = do
        pathUrl <- maybe (Left "") Right $ URL.importURL litteralPath
        case URL.url_type pathUrl of
          URL.Absolute h ->
            case URL.protocol h of
              URL.RawProt "s3" ->
                S3Obj <$> (refuseVarRefs "bucket" $ URL.host h)
                      <*> (parseLocStringAndExt $ URL.url_path pathUrl)
              p -> Left $ "Unsupported protocol: " ++ show p
          URL.HostRelative -> LocalFile <$> (parseLocStringAndExt $ "/" ++ URL.url_path pathUrl)
          URL.PathRelative -> LocalFile <$> (parseLocStringAndExt $ URL.url_path pathUrl)

instance (IsLocPath a) => IsString (Loc_ a) where
  fromString s = case parseURL s of
    Right l -> l
    Left e  -> error e

instance (IsLocPath a, Show a) => Representable (Loc_ a) where
  toTextRepr = T.pack . show
  fromTextRepr x = case parseURL . T.unpack $ x of
    Left _   -> empty
    Right x' -> pure x'

-- | Appends a path to a location. The Loc is considered to be a folder, so its
-- possible extension will be /ignored/.
(</>) :: (IsLocPath a) => Loc_ a -> FilePath -> Loc_ a
f </> p = f & over (locFilePath . pathWithoutExt) (`appendAfterPath` p')
            & locFilePath . pathExtension .~ newExt
  where (p', newExt) = splitExtension' p
infixl 4 </>

-- | Alias for '</>'
(<//>) :: (IsLocPath a) => Loc_ a -> FilePath -> Loc_ a
(<//>) = (</>)
infixl 4 <//>

-- | Replaces a Loc extension
(-<.>) :: Loc -> String -> Loc
f -<.> ext = f & locFilePath . pathExtension .~ ext
infixl 3 -<.>

-- | Initialises a directory from a Loc to it, so that we can safely write in it
-- afterwards. For a local filesystem, this means creating it.
initDir :: Loc -> IO ()
initDir f@(LocalFile{}) =
  Dir.createDirectoryIfMissing True $ f ^. locFilePath . pathWithoutExt
initDir S3Obj{} = pure ()

-- -- | Open the selected folder in a default application
-- openFolder :: Loc -> IO ()
-- openFolder = print

-- | Analog to 'Path.takeDirectory' for generalized locations
takeDirectory :: Loc -> Loc
takeDirectory = over (locFilePath . pathWithoutExt) Path.takeDirectory . dropExtension

-- | Analog of 'Path.dropExtension'
dropExtension :: Loc_ a -> Loc_ a
dropExtension f = f & locFilePath . pathExtension .~ ""

-- | Sets the extension unless the 'Loc_' already has one
addExtToLocIfMissing :: Loc_ a -> String -> Loc_ a
addExtToLocIfMissing loc newExt =
  loc & over locExt (\curExt -> case curExt of
                        "" -> newExt
                        _  -> curExt)
