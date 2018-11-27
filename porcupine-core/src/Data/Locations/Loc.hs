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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}

module Data.Locations.Loc where

import           Control.Applicative
import           Control.Lens
import           Control.Monad              (foldM)
import           Control.Funflow.ContentHashable
import           Data.Aeson
import           Data.Binary                (Binary)
import qualified Data.HashMap.Strict        as HM
import           Data.Locations.LocVariable
import           Data.Representable
import           Data.Store (Store)
import           Data.String
import qualified Data.Text                  as T
import           GHC.Generics               (Generic)
import qualified Network.URL                as URL
import qualified System.Directory           as Dir (createDirectoryIfMissing)
import qualified System.FilePath            as Path


-- | Each location bit can be a simple chunk of string, or a variable name
-- waiting to be spliced in.
data LocBit
  = LocBitChunk FilePath  -- ^ A raw filepath part, to be used as is
  | LocBitVarRef LocVariable -- ^ A variable name
  deriving (Eq, Generic, ToJSON, FromJSON, Store)

instance Show LocBit where
  show (LocBitChunk s)                = s
  show (LocBitVarRef (LocVariable v)) = "{" ++ v ++ "}"

locBitContent :: Lens' LocBit String
locBitContent f (LocBitChunk p) = LocBitChunk <$> f p
locBitContent f (LocBitVarRef (LocVariable v)) = LocBitVarRef . LocVariable <$> f v

-- | A newtype so that we can redefine the Show instance
newtype LocString = LocString [LocBit]
  deriving (Generic, Store)

instance Semigroup LocString where
  LocString l1 <> LocString l2 = LocString $ concatLocBitChunks $ l1++l2
instance Monoid LocString where
  mempty = LocString []

instance Show LocString where
  show (LocString l) = concatMap show l

-- | Get all the variable names still in the loc string and possibly replace
-- them.
locStringVariables :: Traversal' LocString LocBit
locStringVariables f (LocString bits) = LocString . concatLocBitChunks <$> traverse f' bits
  where f' c@(LocBitChunk{})  = pure c
        f' c@(LocBitVarRef{}) = f c

-- | Ensures 2 consecutive chunks are concatenated together
concatLocBitChunks :: [LocBit] -> [LocBit]
concatLocBitChunks (LocBitChunk p1 : LocBitChunk p2 : rest) =
  concatLocBitChunks (LocBitChunk (p1++p2) : rest)
concatLocBitChunks (x : rest) = x : concatLocBitChunks rest
concatLocBitChunks [] = []

data LocFilePath a = LocFilePath { _pathWithoutExt :: a, _pathExtension :: String }
  deriving (Eq, Ord, Generic, ToJSON, FromJSON, Functor, Foldable, Traversable, Binary, Store)

instance (Monad m, ContentHashable m a) => ContentHashable m (LocFilePath a)

makeLenses ''LocFilePath

firstNonEmptyExt :: String -> String -> String
firstNonEmptyExt "" b = b
firstNonEmptyExt a _  = a

instance (Semigroup a) => Semigroup (LocFilePath a) where
  -- Concats the filepaths /without considering extension/ and then chooses one
  -- non-empty extension, right-biased.
  LocFilePath p e <> LocFilePath p' e' =
    LocFilePath (p<>p') $ firstNonEmptyExt e' e
instance (Monoid a) => Monoid (LocFilePath a) where
  mempty = LocFilePath mempty ""

-- | Turns the 'LocFilePath' to/from a simple string to be used as is.
locFilePathAsRawFilePath :: (IsLocString a) => Iso' (LocFilePath a) FilePath
locFilePathAsRawFilePath = iso to_ from_
  where
    to_ (LocFilePath p e) = case e of
      "" -> p'
      _  -> p'++"."++e
      where p' = p ^. locStringAsRawString
    from_ fp = let (p,e) = splitExtension' fp
               in LocFilePath (p ^. from locStringAsRawString) e

instance (IsLocString a) => IsString (LocFilePath a) where
  fromString p = p ^. from locFilePathAsRawFilePath

instance (IsLocString a) => Show (LocFilePath a) where
  show p = fmap (view locStringAsRawString) p ^. locFilePathAsRawFilePath

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
  deriving ( Eq, Ord, Generic, ToJSON, FromJSON
           , Functor, Foldable, Traversable, Binary, Store )

instance (Monad m, ContentHashable m a) => ContentHashable m (Loc_ a)

instance (IsLocString a) => Show (Loc_ a) where
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

type LocalFilePath = LocFilePath String

-- | Creates a 'Loc' from a simple litteral string
localFile :: FilePath -> Loc
localFile s = LocalFile $ s ^. from locFilePathAsRawFilePath

-- | Creates a 'LocWithVars' that will only contain a chunk, no variables
locWithVarsFromLoc :: Loc -> LocWithVars
locWithVarsFromLoc = fmap (LocString . (:[]) . LocBitChunk)

-- | All the variables leftover in a 'LocWithVars'
locVariables :: Traversal' LocWithVars LocBit
locVariables = traversed . locStringVariables

-- | A map that can be used to splice variables in a 'LocWithVars'
type LocVariableMap = HM.HashMap LocVariable String

-- | Splices in the variables present in the hashmap
spliceLocVariables :: LocVariableMap -> LocWithVars -> LocWithVars
spliceLocVariables vars = over locVariables $ \v -> case v of
  LocBitVarRef vname ->
    case HM.lookup vname vars of
      Just val -> LocBitChunk val
      Nothing  -> v
  _ -> error "spliceLocVariables: Should not happen"

terminateLocWithVars :: LocWithVars -> Either String Loc
terminateLocWithVars = traverse terminateLocString
  where
    terminateLocString (LocString [LocBitChunk s]) = Right s
    terminateLocString locString = Left $
      "Variable(s) " ++ show (locString ^.. locStringVariables)
      ++ " in '" ++ show locString ++ "' haven't been given a value"

-- | Means that @a@ can represent file paths
class (Monoid a) => IsLocString a where
  locStringAsRawString :: Iso' a String
  parseLocStringAndExt :: String -> Either String (LocFilePath a)

splitExtension' :: FilePath -> (FilePath, String)
splitExtension' fp = let (f,e) = Path.splitExtension fp in
  case e of '.':e' -> (f,e')
            _      -> (f,e)

instance IsLocString FilePath where
  locStringAsRawString = id
  parseLocStringAndExt fp = Right $ fp ^. from locFilePathAsRawFilePath

parseLocString :: String -> Either String LocString
parseLocString s = (LocString . reverse . map (over locBitContent reverse) . filter isFull)
                   <$> foldM oneChar [] s
  where
    oneChar (LocBitVarRef _ : _) '{' = Left "Cannot nest {...}"
    oneChar acc '{' = return $ LocBitVarRef (LocVariable "") : acc
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

instance IsLocString LocString where
  locStringAsRawString = iso show from_
    where from_ s = LocString [LocBitChunk s]
  parseLocStringAndExt s =
    LocFilePath <$> parseLocString p <*> refuseVarRefs "extension" e
    where (p, e) = splitExtension' s

-- | The main way to parse a 'Loc_'.
parseURL :: (IsLocString a) => String -> Either String (Loc_ a)
parseURL "." = Right $ LocalFile $ LocFilePath ("." ^. from locStringAsRawString) ""
parseURL litteralPath = do
  pathUrl <- maybe (Left $ "parseURL: Invalid URL '" ++ litteralPath ++ "'") Right $
             URL.importURL litteralPath
  case URL.url_type pathUrl of
    URL.Absolute h ->
      case URL.protocol h of
        URL.RawProt "s3" ->
          S3Obj <$> (refuseVarRefs "bucket" $ URL.host h)
                <*> (parseLocStringAndExt $ URL.url_path pathUrl)
        p -> Left $ "Unsupported protocol: " ++ show p
    URL.HostRelative -> LocalFile <$> (parseLocStringAndExt $ "/" ++ URL.url_path pathUrl)
    URL.PathRelative -> LocalFile <$> (parseLocStringAndExt $ URL.url_path pathUrl)

instance (IsLocString a) => IsString (Loc_ a) where
  fromString s = case parseURL s of
    Right l -> l
    Left e  -> error e

instance (IsLocString a) => Representable (LocFilePath a) where
  toTextRepr = T.pack . show
  fromTextRepr x = case parseLocStringAndExt $ T.unpack x of
    Left _   -> empty
    Right x' -> pure x'

instance (IsLocString a) => Representable (Loc_ a) where
  toTextRepr = T.pack . show
  fromTextRepr x = case parseURL $ T.unpack x of
    Left _   -> empty
    Right x' -> pure x'

-- | The equivalent of </> from `filepath` package on 'LocFilePath's
appendToLocFilePathAsSubdir :: (IsLocString a) => LocFilePath a -> String -> LocFilePath a
fp `appendToLocFilePathAsSubdir` s = fp <> (('/':s) ^. from locFilePathAsRawFilePath)

-- | Appends a path to a location. The Loc is considered to be a folder, so its
-- possible extension will be /ignored/.
(</>) :: (IsLocString a) => Loc_ a -> String -> Loc_ a
f </> p = f & over locFilePath (`appendToLocFilePathAsSubdir` p)
infixl 4 </>

-- | Alias for '</>'
(<//>) :: (IsLocString a) => Loc_ a -> String -> Loc_ a
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

-- | Analog to 'Path.takeDirectory' for generalized locations
takeDirectory :: Loc -> Loc
takeDirectory = over (locFilePath . pathWithoutExt) Path.takeDirectory . dropExtension

-- | Analog of 'Path.dropExtension'
dropExtension :: Loc_ a -> Loc_ a
dropExtension f = f & locFilePath . pathExtension .~ ""

-- | Sets the extension unless the 'Loc_' already has one
addExtToLocIfMissing :: Loc_ a -> String -> Loc_ a
addExtToLocIfMissing loc newExt = loc & over locExt (`firstNonEmptyExt` newExt)
