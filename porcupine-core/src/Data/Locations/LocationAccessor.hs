{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes #-}

module Data.Locations.LocationAccessor
  ( LocationAccessor(..)
  , LocationAccessors
  , Rec(..), ElField(..)
  , SomeLocationAccessor(..)
  , accessor
  , runPorcupineM
  , withParsedLocs
  ) where

import           Control.Lens                 ((^.))
-- import           Control.Funflow.ContentHashable
import           Control.Monad.Catch
import           Control.Monad.IO.Unlift
import           Control.Monad.ReaderSoup
import           Control.Monad.ReaderSoup.Resource ()
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import           Data.Aeson
import qualified Data.ByteString.Streaming    as BSS
import           Data.Locations.Loc
import qualified Data.Locations.LocationMonad as LM
import           Data.Locations.LogAndErrors
-- import           Data.Store                      (Store)
import           GHC.TypeLits
import           Network.AWS (MonadAWS)
import qualified System.FilePath                       as Path
import qualified System.IO.Temp                        as Tmp


-- | Creates some Loc type, indexed over a symbol (see ReaderSoup for how that
-- symbol should be used), and equipped with functions to access it in some
-- Monad
class ( MonadMask m, MonadIO m
      , FromJSON (LocOf l), ToJSON (LocOf l) )
   => LocationAccessor (l::Symbol) m where

  type IsLocationAccessor_ l m :: Bool
  type IsLocationAccessor_ l m = 'True
  
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
        readBSS (L (localFile tmpFile)) (writeBSS loc)
        return res

-- | Accessing local resources
instance (MonadResource m, MonadMask m) => LocationAccessor "resource" m where
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

-- TODO: Move "aws" instance to its own porcupine-s3 package

-- | Accessing resources on S3
instance (MonadAWS m, MonadMask m, MonadResource m) => LocationAccessor "aws" m where
  newtype LocOf "aws" = S Loc
    deriving (FromJSON, ToJSON)
  locExists _ = return True -- TODO: Implement it
  writeBSS (S l) = LM.writeBSS_S3 l
  readBSS (S l) f = LM.readBSS_S3 l f -- >>= LM.eitherToExn
  copy (S l1) (S l2) = LM.copy_S3 l1 l2 -- >>= LM.eitherToExn

-- | Packs together all the information necessary to run a context and access
-- locations related to that context
data LocationAccessorRunner m (a::(Symbol,*)) where
  LocationAccessorRunner :: ( LocationAccessor l m
                            , CanRunSoupContext l args t m )
                         => ElField '(l,args) -> LocationAccessorRunner m '(l,args)

type LocationAccessors m = Rec (LocationAccessorRunner m)

accessor :: (KnownSymbol l, LocationAccessor l m, CanRunSoupContext l args t m)
         => Label l -> args -> LocationAccessorRunner m '(l,args)
accessor lbl args = LocationAccessorRunner $ lbl =: args

-- | Reifies an instance of LocationAccessor
data SomeLocationAccessor m where
  SomeLocationAccessor :: (LocationAccessor l m)
                       => Label l -> SomeLocationAccessor m

-- | All the LocationAccessors available to the system during a run, so that
-- when we encounter an Aeson Value corresponding to some LocOf, we may try them
-- all and use the first one that matches.
newtype LocParserCtx m = LocParserCtx [SomeLocationAccessor m]


-- * Retreiving the list of all available LocationAccessors

-- NOTE: This part of the code (looking up a type-level list to find the types
-- that satisfy the LocationAccessor constraint) is probably a bit
-- over-engineered currently. We should find a way to make it leaner.

type family IsDefinedTrue a where
  IsDefinedTrue 'True = 'True
  IsDefinedTrue a = 'False

type IsLocationAccessor l m = IsDefinedTrue (IsLocationAccessor_ l m)

type family LAWitnesses m ctxs :: [Bool] where
  LAWitnesses m '[] = '[]
  LAWitnesses m ('(l,a) : ctxs) =
    IsLocationAccessor l m : LAWitnesses m ctxs

class (laWitnesses ~ LAWitnesses m ctxs)
   => GetLocParserCtx m ctxs laWitnesses where
  getLocParserCtx :: Rec ElField ctxs -> LocParserCtx m

instance GetLocParserCtx m '[] '[] where
  getLocParserCtx _ = LocParserCtx []

instance (GetLocParserCtx m ctxs ws, IsLocationAccessor l m ~ 'False)
      => GetLocParserCtx m ('(l,a) : ctxs) ('False : ws) where
  getLocParserCtx (_ :& ctxs) = getLocParserCtx ctxs

instance (GetLocParserCtx m ctxs ws, LocationAccessor l m, IsLocationAccessor l m ~ 'True)
      => GetLocParserCtx m ('(l,a) : ctxs) ('True : ws) where
  getLocParserCtx (_ :& ctxs) = LocParserCtx $ SomeLocationAccessor (fromLabel @l) : rest
    where LocParserCtx rest = getLocParserCtx ctxs

-- * The monad the application runs in, and how to run it

type PorcupineM ctxs = ReaderT (LocParserCtx (ReaderSoup ctxs)) (ReaderSoup ctxs)

runPorcupineM :: (ArgsForSoupConsumption args
                 ,GetLocParserCtx (ReaderSoup (CtxsFromArgs args)) args
                   (LAWitnesses (ReaderSoup (CtxsFromArgs args)) args))
              => Rec ElField args
              -> PorcupineM (CtxsFromArgs args) a
              -> IO a
runPorcupineM argRecord act = consumeSoup argRecord $ runReaderT act parserCtx
  where parserCtx = getLocParserCtx argRecord

withParsedLocs :: (KatipContext (PorcupineM ctxs))
               => [Value]
               -> (forall l. (LocationAccessor l (ReaderSoup ctxs))
                   => [LocOf l] -> PorcupineM ctxs r)
               -> PorcupineM ctxs r
withParsedLocs aesonVals f = do
  LocParserCtx allAccessors <- ask
  loop allAccessors []
  where
    loop [] errs = throwWithPrefix $ "Locations '" ++ show aesonVals
                   ++ "': Accessors returned the following errors: "
                   ++ show (reverse errs)
    loop (SomeLocationAccessor (_ :: Label l) : accs) errs =
      case mapM fromJSON aesonVals of
        Success a -> f (a::[LocOf l])
        Error e -> loop accs (e:errs)

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
