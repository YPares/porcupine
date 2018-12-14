{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Data.Locations.Accessors
  ( module Control.Monad.ReaderSoup.Resource
  , FromJSON(..), ToJSON(..)
  , LocationAccessor(..)
  , Rec(..), ElField(..)
  , MayProvideLocationAccessors(..)
  , SomeLocationAccessor(..)
  , AvailableAccessors
  , PorcupineM, SimplePorcupineM
  , (<--)
  , runPorcupineM
  , withParsedLocs
  ) where

import           Control.Lens                      (over, (^.), _1)
-- import           Control.Funflow.ContentHashable
import           Control.Monad.Catch
import           Control.Monad.IO.Unlift
import           Control.Monad.ReaderSoup
import           Control.Monad.ReaderSoup.Resource
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import           Data.Aeson
import qualified Data.ByteString.Streaming         as BSS
import           Data.Locations.Loc
import qualified Data.Locations.LocationMonad      as LM
import           Data.Locations.LogAndErrors
-- import           Data.Store                      (Store)
import           Data.Vinyl
import           Data.Vinyl.Functor
import           GHC.TypeLits
import qualified System.FilePath                   as Path
import qualified System.IO.Temp                    as Tmp


-- | Creates some Loc type, indexed over a symbol (see ReaderSoup for how that
-- symbol should be used), and equipped with functions to access it in some
-- Monad
class ( MonadMask m, MonadIO m
      , FromJSON (LocOf l), ToJSON (LocOf l) )
   => LocationAccessor m (l::Symbol) where

  data LocOf l :: *

  locExists :: LocOf l -> m Bool

  writeBSS :: LocOf l -> BSS.ByteString m r -> m r

  readBSS :: LocOf l -> (BSS.ByteString m () -> m b) -> m (Either LM.Error b)

  copy :: LocOf l -> LocOf l -> m (Either LM.Error ())
  copy locFrom locTo = readBSS locFrom (writeBSS locTo)

  withLocalBuffer :: (FilePath -> m a) -> LocOf l -> m a
  -- If we have a local resource accessor, we use it:
  default withLocalBuffer :: (MonadResource m)
                          => (FilePath -> m a) -> LocOf l -> m a
  withLocalBuffer f loc =
    Tmp.withSystemTempDirectory "pipeline-tools-tmp" writeAndUpload
    where
      writeAndUpload tmpDir = do
        let tmpFile = tmpDir Path.</> "out"
        res <- f tmpFile
        _ <- readBSS (L (localFile tmpFile)) (writeBSS loc)
        return res

-- | Reifies an instance of LocationAccessor
data SomeLocationAccessor m where
  SomeLocationAccessor :: (LocationAccessor m l)
                       => Label l -> SomeLocationAccessor m

-- | This class is meant to be implemented by every label used in the reader
-- soup. It tells whether this label provides LocationAccessors (usually zero or
-- 1).
class MayProvideLocationAccessors m l where
  getLocationAccessors :: Label l -> [SomeLocationAccessor m]
  default getLocationAccessors :: (LocationAccessor m l) => Label l -> [SomeLocationAccessor m]
  getLocationAccessors x = [SomeLocationAccessor x]

-- | By default, no accessor is provided
instance {-# OVERLAPPABLE #-} MayProvideLocationAccessors m l where
  getLocationAccessors _ = []

-- | Packs together the args to run a context of the ReaderSoup, and if
-- available, an instance of LocationAccessor
type FieldWithAccessors m =
  Compose ((,) [SomeLocationAccessor m]) ElField

-- | Much like (=:) builds an ElField, (<--) builds a Field composed with
-- LocationAccessors (if available)
(<--) :: (KnownSymbol l, MayProvideLocationAccessors m l)
      => Label l -> args -> FieldWithAccessors m (l:::args)
lbl <-- args = Compose (getLocationAccessors lbl, lbl =: args)

-- | All the LocationAccessors available to the system during a run, so that
-- when we encounter an Aeson Value corresponding to some LocOf, we may try them
-- all and use the first one that matches.
newtype AvailableAccessors m = AvailableAccessors [SomeLocationAccessor m]

-- | Retrieves the list of all available LocationAccessors
splitAccessorsFromRec ::
  Rec (FieldWithAccessors m) ctxs -> (AvailableAccessors m, Rec ElField ctxs)
splitAccessorsFromRec = over _1 AvailableAccessors . rtraverse getCompose
  -- `(,) a` is an Applicative if a is a Monoid, so this will merge all the lists
  -- of SomeLocationAccessors

-- * Making "resource" a LocationAccessor

-- | Accessing local resources
instance (MonadResource m, MonadMask m) => LocationAccessor m "resource" where
  newtype LocOf "resource" = L Loc
    deriving (FromJSON, ToJSON)
  locExists (L l) = LM.checkLocal "locExists" LM.locExists_Local l
  writeBSS (L l) = LM.checkLocal "writeBSS" LM.writeBSS_Local l
  readBSS (L l) f =
    LM.checkLocal "readBSS" (\l' -> LM.readBSS_Local l' f {->>= LM.eitherToExn-}) l
  withLocalBuffer f (L l) =
    LM.checkLocal "withLocalBuffer" (\l' -> f $ l'^.locFilePathAsRawFilePath) l
  copy (L l1) (L l2) =
    LM.checkLocal "copy" (\file1 ->
      LM.checkLocal "copy (2nd argument)" (LM.copy_Local file1) l2) l1
    -- >>= LM.eitherToExn

instance (MonadResource m, MonadMask m) => MayProvideLocationAccessors m "resource"


--- The rest of the file is used as a compatiblity layer with the LocationMonad
--- class

-- | Temporary type until LocationMonad is removed.
type PorcupineM ctxs = ReaderT (AvailableAccessors (ReaderSoup ctxs)) (ReaderSoup ctxs)

-- | The simplest, minimal ReaderSoup to run a local accessor
type SimplePorcupineM =
  PorcupineM '[ "katip" ::: ContextFromName "katip"
              , "resource" ::: ContextFromName "resource" ]

-- | Temporary runner until LocationMonad is removed
runPorcupineM :: (ArgsForSoupConsumption args)
              => Rec (FieldWithAccessors (ReaderSoup (ContextsFromArgs args))) args
              -> PorcupineM (ContextsFromArgs args) a
              -> IO a
runPorcupineM argsWithAccsRec act = consumeSoup argsRec $ runReaderT act parserCtx
  where (parserCtx, argsRec) = splitAccessorsFromRec argsWithAccsRec

-- | Finds in the accessors list a way to parse a list of JSON values that
-- should correspond to some `LocOf l` type
withParsedLocs :: (KatipContext (PorcupineM ctxs))
               => [Value]
               -> (forall l. (LocationAccessor (ReaderSoup ctxs) l)
                   => [LocOf l] -> PorcupineM ctxs r)
               -> PorcupineM ctxs r
withParsedLocs aesonVals f = do
  AvailableAccessors allAccessors <- ask
  case allAccessors of
    [] -> throwWithPrefix $ "List of accessors is empty"
    _  -> return ()
  loop allAccessors []
  where
    loop [] errs = throwWithPrefix $ "Locations '" ++ show aesonVals
                   ++ "': Accessors returned the following errors: "
                   ++ show (reverse errs)
    loop (SomeLocationAccessor (_ :: Label l) : accs) errs =
      case mapM fromJSON aesonVals of
        Success a -> f (a::[LocOf l])
        Error e   -> loop accs (e:errs)

-- Temporary, until LocationMonad is removed and LocOf types are directly
-- integrated into the resource tree:
instance (KatipContext (ReaderSoup ctxs)) => LM.LocationMonad (PorcupineM ctxs) where
  locExists l = withParsedLocs [toJSON l] $ lift . locExists . head
  writeBSS l bs = withParsedLocs [toJSON l] $ \[l'] -> do
    lpc <- ask
    lift $ writeBSS l' $ hoist (flip runReaderT lpc) bs
  readBSS l f = withParsedLocs [toJSON l] $ \[l'] -> do
    lpc <- ask
    lift $ readBSS l' (flip runReaderT lpc . f . hoist (ReaderT . const))
  copy l1 l2 = withParsedLocs [toJSON l1, toJSON l2] $ \[l1', l2'] -> do
    lift $ copy l1' l2'
