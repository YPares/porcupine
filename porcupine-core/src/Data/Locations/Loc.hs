{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
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
import           Data.Typeable
import           GHC.Generics                    (Generic)
import qualified Network.URL                     as URL
import qualified System.Directory                as Dir (createDirectoryIfMissing)
import qualified System.FilePath                 as Path


-- | Each location bit can be a simple chunk of string, or a variable name
-- waiting to be spliced in.
data StringOrVariable
  = SoV_String FilePath  -- ^ A raw filepath part, to be used as is
  | SoV_Variable LocVariable -- ^ A variable name
  deriving (Eq, Generic, ToJSON, FromJSON, Store)

instance Show StringOrVariable where
  show (SoV_String s)                = s
  show (SoV_Variable (LocVariable v)) = "{" ++ v ++ "}"

locBitContent :: Lens' StringOrVariable String
locBitContent f (SoV_String p) = SoV_String <$> f p
locBitContent f (SoV_Variable (LocVariable v)) = SoV_Variable . LocVariable <$> f v

-- | A newtype so that we can redefine the Show instance
newtype StringWithVars = StringWithVars [StringOrVariable]
  deriving (Generic, Store)

instance Semigroup StringWithVars where
  StringWithVars l1 <> StringWithVars l2 = StringWithVars $ concatSoV_Strings $ l1++l2
instance Monoid StringWithVars where
  mempty = StringWithVars []

instance Show StringWithVars where
  show (StringWithVars l) = concatMap show l

-- | Get all the variable names still in the loc string and possibly replace
-- them.
locStringVariables :: Traversal' StringWithVars StringOrVariable
locStringVariables f (StringWithVars bits) = StringWithVars . concatSoV_Strings <$> traverse f' bits
  where f' c@SoV_String{}  = pure c
        f' c@SoV_Variable{} = f c

-- | Ensures 2 consecutive chunks are concatenated together
concatSoV_Strings :: [StringOrVariable] -> [StringOrVariable]
concatSoV_Strings (SoV_String p1 : SoV_String p2 : rest) =
  concatSoV_Strings (SoV_String (p1++p2) : rest)
concatSoV_Strings (x : rest) = x : concatSoV_Strings rest
concatSoV_Strings [] = []

data PathWithExtension a = PathWithExtension { _pathWithoutExt :: a, _pathExtension :: String }
  deriving (Eq, Ord, Generic, ToJSON, FromJSON, Functor, Foldable, Traversable, Binary, Store)

instance (Monad m, ContentHashable m a) => ContentHashable m (PathWithExtension a)

makeLenses ''PathWithExtension

firstNonEmptyExt :: String -> String -> String
firstNonEmptyExt "" b = b
firstNonEmptyExt a _  = a

instance (Semigroup a) => Semigroup (PathWithExtension a) where
  -- Concats the filepaths /without considering extension/ and then chooses one
  -- non-empty extension, right-biased.
  PathWithExtension p e <> PathWithExtension p' e' =
    PathWithExtension (p<>p') $ firstNonEmptyExt e' e
instance (Monoid a) => Monoid (PathWithExtension a) where
  mempty = PathWithExtension mempty ""

-- | Turns the 'PathWithExtension' to/from a simple string to be used as is.
pathWithExtensionAsRawFilePath :: (IsLocString a) => Iso' (PathWithExtension a) FilePath
pathWithExtensionAsRawFilePath = iso to_ from_
  where
    to_ (PathWithExtension p e) = case e of
      "" -> p'
      _  -> p'++"."++e
      where p' = p ^. locStringAsRawString
    from_ fp = let (p,e) = splitExtension' fp
               in PathWithExtension (p ^. from locStringAsRawString) e

instance (IsLocString a) => IsString (PathWithExtension a) where
  fromString p = p ^. from pathWithExtensionAsRawFilePath

instance (IsLocString a) => Show (PathWithExtension a) where
  show p = fmap (view locStringAsRawString) p ^. pathWithExtensionAsRawFilePath

data QParam a = QParam a a
  deriving (Eq, Ord, Generic, Functor, Foldable, Traversable, Binary, Store)

instance (Monad m, ContentHashable m a) => ContentHashable m (QParam a)

instance (IsLocString a) => Show (QParam a) where
  show = show . view (from asQParam)

asQParam :: (IsLocString a) => Iso' (String,String) (QParam a)
asQParam = iso to_ from_
  where
    to_ (x,y) = QParam (x ^. from locStringAsRawString) (y ^. from locStringAsRawString)
    from_ (QParam x y) = (x ^. locStringAsRawString, y ^. locStringAsRawString)

-- | Location's main type. A value of type 'URL' denotes a file or a
-- folder that may be local or hosted remotely
data URL a
  = LocalFile { filePath :: PathWithExtension a }
  | RemoteFile { rfProtocol    :: String
               , rfServerName  :: String
               , rfPortNumber  :: Maybe Integer
               , rfPathWithExtension :: PathWithExtension a
               , rfLocParams   :: [QParam a] }
  deriving ( Eq, Ord, Generic
           , Functor, Foldable, Traversable, Binary, Store )

instance (Monad m, Typeable a, ContentHashable m a) => ContentHashable m (URL a)

instance (IsLocString a) => Show (URL a) where
  show LocalFile{ filePath } = show filePath
  show RemoteFile{ rfProtocol, rfServerName, rfPathWithExtension, rfPortNumber, rfLocParams } =
    rfProtocol ++ "://" ++ rfServerName ++ port ++ "/" ++ show rfPathWithExtension ++ qs
    where
      port = case rfPortNumber of
        Nothing -> ""
        Just p  -> ":" <> show p
      qs = case rfLocParams of
        [] -> ""
        _  -> "?" ++ URL.exportParams (map (view (from asQParam)) rfLocParams)

urlPathWithExtension :: Lens' (URL a) (PathWithExtension a)
urlPathWithExtension f (LocalFile fp)                  = LocalFile <$> f fp
urlPathWithExtension f RemoteFile{rfPathWithExtension=fp,..} =
  (\fp' -> RemoteFile{rfPathWithExtension=fp',..}) <$> f fp

-- | A 'URL' that might contain some named holes, called variables, that we
-- have first to replace by a value before we can get a definite physical
-- location.
type LocWithVars = URL StringWithVars

-- | A 'URL' that can directly be accessed as is.
type Loc = URL String

type LocalFilePath = PathWithExtension String

-- | Creates a 'Loc' from a simple litteral string
localFile :: FilePath -> Loc
localFile s = LocalFile $ s ^. from pathWithExtensionAsRawFilePath

-- | Creates a 'LocWithVars' that will only contain a chunk, no variables
locWithVarsFromLoc :: (Functor f) => f String -> f StringWithVars
locWithVarsFromLoc = fmap (StringWithVars . (:[]) . SoV_String)

-- | A map that can be used to splice variables in a 'LocWithVars'
type LocVariableMap = HM.HashMap LocVariable String

-- | Splices in the variables present in the hashmap
spliceLocVariables :: (Functor f) => LocVariableMap -> f StringWithVars -> f StringWithVars
spliceLocVariables vars = fmap $ over locStringVariables $ \v -> case v of
  SoV_Variable vname ->
    case HM.lookup vname vars of
      Just val -> SoV_String val
      Nothing  -> v
  _ -> error "spliceLocVariables: Should not happen"

-- | Yields @Left _@ if any of the given StringWithVars contains variables.
terminateLocWithVars :: (Traversable f) => f StringWithVars -> Either String (f String)
terminateLocWithVars = traverse terminateStringWithVars
  where
    terminateStringWithVars (StringWithVars [SoV_String s]) = Right s
    terminateStringWithVars locString = Left $
      "Variable(s) " ++ show (locString ^.. locStringVariables)
      ++ " in '" ++ show locString ++ "' haven't been given a value"

-- | Means that @a@ can represent file paths
class (Monoid a) => IsLocString a where
  locStringAsRawString :: Iso' a String
  parseLocString       :: String -> Either String a

parseLocStringAndExt :: (IsLocString a) => String -> Either String (PathWithExtension a)
parseLocStringAndExt s =
  PathWithExtension <$> parseLocString p <*> refuseVarRefs "extension" e
    where (p, e) = splitExtension' s

splitExtension' :: FilePath -> (FilePath, String)
splitExtension' fp = let (f,e) = Path.splitExtension fp in
  case e of '.':e' -> (f,e')
            _      -> (f,e)

instance IsLocString String where
  locStringAsRawString = id
  parseLocString = Right

parseStringWithVars :: String -> Either String StringWithVars
parseStringWithVars s = (StringWithVars . reverse . map (over locBitContent reverse) . filter isFull)
                   <$> foldM oneChar [] s
  where
    oneChar (SoV_Variable _ : _) '{' = Left "Cannot nest {...}"
    oneChar acc '{' = return $ SoV_Variable (LocVariable "") : acc
    oneChar (SoV_String _ : _) '}' = Left "'}' terminates nothing"
    oneChar acc '}' = return $ SoV_String "" : acc
    oneChar (hd : rest) c = return $ over locBitContent (c:) hd : rest
    oneChar [] c = return [SoV_String [c]]

    isFull (SoV_String "") = False
    isFull _               = True

-- | @refuseVarRefs p s == Right s@ if `s` contains no variables.
-- Otherwise, yields an error message.
refuseVarRefs :: String -> String -> Either String String
refuseVarRefs place s = do
  l <- parseStringWithVars s
  case l of
    (StringWithVars []) -> return ""
    (StringWithVars [SoV_String p]) -> return p
    _ -> Left $ "Variable references {...} are not allowed in the " ++ place ++ " part of a URL"

instance IsLocString StringWithVars where
  locStringAsRawString = iso show from_
    where from_ s = StringWithVars [SoV_String s]
  parseLocString = parseStringWithVars

-- | The main way to parse an 'URL'. Variables are not allowed in the protocol
-- and server parts.
parseURL :: (IsLocString a) => String -> Either String (URL a)
parseURL "." = Right $ LocalFile $ PathWithExtension ("." ^. from locStringAsRawString) ""
parseURL litteralPath = do
  url <- maybe (Left $ "parseURL: Invalid URL '" ++ litteralPath ++ "'") Right $
             URL.importURL litteralPath
  case URL.url_type url of
    URL.Absolute h ->
       RemoteFile <$> (refuseVarRefs "protocol" $ getProtocol $ URL.protocol h)
                  <*> (refuseVarRefs "server" $ URL.host h)
                  <*> (Right $ URL.port h)
                  <*> (parseLocStringAndExt $ URL.url_path url)
                  <*> (map (uncurry QParam) <$>
                         mapMOf (traversed.both) parseLocString (URL.url_params url))
    URL.HostRelative -> LocalFile <$> (parseLocStringAndExt $ "/" ++ URL.url_path url)
    URL.PathRelative -> LocalFile <$> (parseLocStringAndExt $ URL.url_path url)
  where getProtocol (URL.RawProt h)  = map toLower h
        getProtocol (URL.HTTP False) = "http"
        getProtocol (URL.HTTP True)  = "https"
        getProtocol (URL.FTP False)  = "ftp"
        getProtocol (URL.FTP True)   = "ftps"

instance (IsLocString a) => IsString (URL a) where
  fromString s = case parseURL s of
    Right l -> l
    Left e  -> error e

instance (IsLocString a) => Representable (PathWithExtension a) where
  toTextRepr = T.pack . show
  fromTextRepr x = case parseLocStringAndExt $ T.unpack x of
    Left _   -> empty
    Right x' -> pure x'

instance (IsLocString a) => Representable (URL a) where
  toTextRepr = T.pack . show
  fromTextRepr x = case parseURL $ T.unpack x of
    Left _   -> empty
    Right x' -> pure x'

instance (IsLocString a) => FromJSON (URL a) where
  parseJSON (String j) = fromTextRepr j
  parseJSON _          = fail "URL must be read from a JSON String"

instance (IsLocString a) => ToJSON (URL a) where
  toJSON = String . toTextRepr

-- | The equivalent of </> from `filepath` package on 'PathWithExtension's
appendToPathWithExtensionAsSubdir :: (IsLocString a) => PathWithExtension a -> String -> PathWithExtension a
fp `appendToPathWithExtensionAsSubdir` s = view (from pathWithExtensionAsRawFilePath) $
    (fp^.pathWithExtensionAsRawFilePath) Path.</> s

-- | Appends a path to a location. The Loc is considered to be a folder, so its
-- possible extension will be /ignored/.
(</>) :: (IsLocString a) => URL a -> String -> URL a
f </> p = f & over urlPathWithExtension (`appendToPathWithExtensionAsSubdir` p)
infixl 4 </>

-- | Alias for '</>'
(<//>) :: (IsLocString a) => URL a -> String -> URL a
(<//>) = (</>)
infixl 4 <//>

-- | Replaces a Loc extension
(-<.>) :: Loc -> String -> Loc
f -<.> ext = f & urlPathWithExtension . pathExtension .~ ext
infixl 3 -<.>

-- | Initialises a directory from a Loc to it, so that we can safely write in it
-- afterwards. For a local filesystem, this means creating it.
initDir :: Loc -> IO ()
initDir f@LocalFile{} =
  Dir.createDirectoryIfMissing True $ f ^. urlPathWithExtension . pathWithoutExt
initDir _ = pure ()

-- | Analog to 'Path.takeDirectory' for generalized locations
takeDirectory :: Loc -> Loc
takeDirectory = over (urlPathWithExtension . pathWithoutExt) Path.takeDirectory . dropExtension

-- | Analog of 'Path.dropExtension'
dropExtension :: URL a -> URL a
dropExtension f = f & urlPathWithExtension . pathExtension .~ ""

-- | The class of all locations that can be mapped to VirtualFiles in a
-- configuration file.
class (Traversable f
      -- Just ensure that `forall a. (IsLocString a) => (FromJSON (f a),
      -- ToJSON (f a))`:
      ,FromJSON (f String), FromJSON (f StringWithVars)
      ,ToJSON (f String), ToJSON (f StringWithVars)
      -- `forall a. (IsLocString a) => (Show (f a))`:
      ,Show (f String), Show (f StringWithVars)) => TypedLocation f where

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

  -- | Apply a mapping shortcut (represented as a partial file path, with its
  -- extension) to the location. For now, non URL-based locations should send an
  -- error
  --
  -- Note: contrary to 'addSubdirToLoc', the 'PathWithExtension' MAY contain
  -- slashes
  useLocAsPrefix :: (IsLocString a) => f a -> PathWithExtension a -> f a

instance TypedLocation URL where
  setLocType l f = l & over (urlPathWithExtension . pathExtension) f
  getLocType = view (urlPathWithExtension . pathExtension)
  addSubdirToLoc = (</>)
  useLocAsPrefix l p = l & over urlPathWithExtension (<> p)

-- | Sets the file type of a location
overrideLocType :: (TypedLocation f) => f a -> String -> f a
overrideLocType loc newExt = setLocType loc (newExt `firstNonEmptyExt`)

-- | Sets the file type of a location unless it already has one
setLocTypeIfMissing :: (TypedLocation f) => f a -> String -> f a
setLocTypeIfMissing loc newExt = setLocType loc (`firstNonEmptyExt` newExt)
