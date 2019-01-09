{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE StaticPointers        #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -Wall #-}

module Data.Locations.Loc where

import           Control.Applicative
import           Control.Funflow.ContentHashable
import           Control.Lens
import           Control.Monad                   (foldM)
import           Data.Aeson
import           Data.Binary                     (Binary)
import           Data.Char                       (toLower)
import qualified Data.HashMap.Strict             as HM
import           Data.Locations.LocVariable
import           Data.Representable
import           Data.Store                      (Store)
import           Data.String
import qualified Data.Text                       as T
import           GHC.Generics                    (Generic)
import qualified Network.URL                     as URL
import qualified System.Directory                as Dir (createDirectoryIfMissing)
import qualified System.FilePath                 as Path


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


-- | Location's main type. A value of type 'URLLikeLoc' denotes a file or a
-- folder that may be local or hosted remotely
data URLLikeLoc a
  = LocalFile { filePath :: LocFilePath a }
  | RemoteFile { rfProtocol    :: String
               , rfServerName  :: String
               , rfLocFilePath :: LocFilePath a }
  deriving ( Eq, Ord, Generic
           , Functor, Foldable, Traversable, Binary, Store )

instance (Monad m, ContentHashable m a) => ContentHashable m (URLLikeLoc a)

instance (IsLocString a) => Show (URLLikeLoc a) where
  show LocalFile{ filePath } = show filePath
  show RemoteFile{ rfProtocol, rfServerName, rfLocFilePath } =
    rfProtocol ++ "://" ++ rfServerName ++ "/" ++ show rfLocFilePath

locFilePath :: Lens (URLLikeLoc a) (URLLikeLoc b) (LocFilePath a) (LocFilePath b)
locFilePath f (LocalFile fp)      = LocalFile <$> f fp
locFilePath f (RemoteFile p b fp) = RemoteFile p b <$> f fp

-- | A 'URLLikeLoc' that might contain some names holes, called variables, that we
-- have first to replace by a value before we can get a definite physical
-- location.
type LocWithVars = URLLikeLoc LocString

-- | A 'URLLikeLoc' that can directly be accessed as is.
type Loc = URLLikeLoc String

type LocalFilePath = LocFilePath String

-- | Creates a 'Loc' from a simple litteral string
localFile :: FilePath -> Loc
localFile s = LocalFile $ s ^. from locFilePathAsRawFilePath

-- | Creates a 'LocWithVars' that will only contain a chunk, no variables
locWithVarsFromLoc :: (Functor f) => f String -> f LocString
locWithVarsFromLoc = fmap (LocString . (:[]) . LocBitChunk)

-- | A map that can be used to splice variables in a 'LocWithVars'
type LocVariableMap = HM.HashMap LocVariable String

-- | Splices in the variables present in the hashmap
spliceLocVariables :: (Functor f) => LocVariableMap -> f LocString -> f LocString
spliceLocVariables vars = fmap $ over locStringVariables $ \v -> case v of
  LocBitVarRef vname ->
    case HM.lookup vname vars of
      Just val -> LocBitChunk val
      Nothing  -> v
  _ -> error "spliceLocVariables: Should not happen"

terminateLocWithVars :: (Traversable f) => f LocString -> Either String (f String)
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

-- | The main way to parse an 'URLLikeLoc'.
parseURLLikeLoc :: (IsLocString a) => String -> Either String (URLLikeLoc a)
parseURLLikeLoc "." = Right $ LocalFile $ LocFilePath ("." ^. from locStringAsRawString) ""
parseURLLikeLoc litteralPath = do
  pathUrl <- maybe (Left $ "parseURLLikeLoc: Invalid URL '" ++ litteralPath ++ "'") Right $
             URL.importURL litteralPath
  case URL.url_type pathUrl of
    URL.Absolute h ->
       RemoteFile <$> (refuseVarRefs "protocol" $ getProtocol $ URL.protocol h)
                  <*> (refuseVarRefs "server" $ URL.host h)
                  <*> (parseLocStringAndExt $ URL.url_path pathUrl)
    URL.HostRelative -> LocalFile <$> (parseLocStringAndExt $ "/" ++ URL.url_path pathUrl)
    URL.PathRelative -> LocalFile <$> (parseLocStringAndExt $ URL.url_path pathUrl)
  where getProtocol (URL.RawProt h)  = map toLower h
        getProtocol (URL.HTTP False) = "http"
        getProtocol (URL.HTTP True)  = "https"
        getProtocol (URL.FTP False)  = "ftp"
        getProtocol (URL.FTP True)   = "ftps"

instance (IsLocString a) => IsString (URLLikeLoc a) where
  fromString s = case parseURLLikeLoc s of
    Right l -> l
    Left e  -> error e

instance (IsLocString a) => Representable (LocFilePath a) where
  toTextRepr = T.pack . show
  fromTextRepr x = case parseLocStringAndExt $ T.unpack x of
    Left _   -> empty
    Right x' -> pure x'

instance (IsLocString a) => Representable (URLLikeLoc a) where
  toTextRepr = T.pack . show
  fromTextRepr x = case parseURLLikeLoc $ T.unpack x of
    Left _   -> empty
    Right x' -> pure x'

instance (IsLocString a) => FromJSON (URLLikeLoc a) where
  parseJSON (String j) = fromTextRepr j
  parseJSON _          = fail "URLLikeLoc must be read from a JSON String"

instance (IsLocString a) => ToJSON (URLLikeLoc a) where
  toJSON = String . toTextRepr

-- | The equivalent of </> from `filepath` package on 'LocFilePath's
appendToLocFilePathAsSubdir :: (IsLocString a) => LocFilePath a -> String -> LocFilePath a
fp `appendToLocFilePathAsSubdir` s = fp <> (('/':s) ^. from locFilePathAsRawFilePath)

-- | Appends a path to a location. The Loc is considered to be a folder, so its
-- possible extension will be /ignored/.
(</>) :: (IsLocString a) => URLLikeLoc a -> String -> URLLikeLoc a
f </> p = f & over locFilePath (`appendToLocFilePathAsSubdir` p)
infixl 4 </>

-- | Alias for '</>'
(<//>) :: (IsLocString a) => URLLikeLoc a -> String -> URLLikeLoc a
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
initDir _ = pure ()

-- | Analog to 'Path.takeDirectory' for generalized locations
takeDirectory :: Loc -> Loc
takeDirectory = over (locFilePath . pathWithoutExt) Path.takeDirectory . dropExtension

-- | Analog of 'Path.dropExtension'
dropExtension :: URLLikeLoc a -> URLLikeLoc a
dropExtension f = f & locFilePath . pathExtension .~ ""

-- | The class of all locations that can be mapped to VirtualFiles
class (Traversable f
      -- Just ensure that `forall a. (IsLocString a) => (FromJSON (GLocOf l a),
      -- ToJSON (GLocOf l a))`:
      ,FromJSON (f String), FromJSON (f LocString)
      ,ToJSON (f String), ToJSON (f LocString)
      -- `forall a. (IsLocString a) => (Show (GLocOf l a))
      ,Show (f String), Show (f LocString)) => TypedLocation f where

  -- TODO: Find a way to replace get/setLocType by a Lens. This displeased
  -- GeneralizedNewtypeDeriving when making LocationAccessor and trying to
  -- automatically derived instances of TypedLocation
  getLocType :: f a -> String
  
  -- | Access the file type part of a location
  --
  -- For locations that encode types as extensions, this would access the
  -- extension. But for others (like HTTP urls), locType would probably need to
  -- translate it first to a mime type, or some other implementation-specific
  -- way to to represent resource types
  setLocType :: f a -> (String -> String) -> f a
  
  -- | Use the location as a directory and append a "subdir" to it. Depending on
  -- the implementation this "subdir" relationship can only be semantic (the
  -- result doesn't have to physically be a subdirectory of the input)
  --
  -- Note: this isn't a path, the subdir shouldn't contain any slashes
  addSubdirToLoc :: (IsLocString a) => f a -> String -> f a

  -- | "Integrate" a mapping shortcut (represented as a partial file path, with
  -- its extension) to the location. For now, non URL-based locations should
  -- send an error
  --
  -- Note: contrary to 'addSubdirToLoc', the filepath MAY contain slashes
  useLocAsPrefix :: (IsLocString a) => f a -> LocFilePath a -> f a
  
instance TypedLocation URLLikeLoc where
  setLocType l f = l & over (locFilePath . pathExtension) f
  getLocType = view (locFilePath . pathExtension)
  addSubdirToLoc = (</>)
  useLocAsPrefix l p = l & over locFilePath (<> p)

-- | Sets the file type of a location
overrideLocType :: (TypedLocation f) => f a -> String -> f a
overrideLocType loc newExt = setLocType loc (newExt `firstNonEmptyExt`)

-- | Sets the file type of a location unless it already has one
addExtToLocIfMissing :: (TypedLocation f) => f a -> String -> f a
addExtToLocIfMissing loc newExt = setLocType loc (`firstNonEmptyExt` newExt)
-- TODO: rename it
