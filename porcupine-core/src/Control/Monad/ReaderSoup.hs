{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Control.Monad.ReaderSoup where

import Control.Lens (over)
import qualified Control.Monad.Reader.Class as MR
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Reader
import Data.Vinyl
import Data.Vinyl.TypeLevel
import GHC.TypeLits
import GHC.OverloadedLabels


-- | Represents a set of Reader-like monads as a one-layer Reader that can grow
-- and host more Readers, in a way that's more generic than creating you own
-- application stack of Reader and implementing a host of MonadXXX classes,
-- because each of these MonadXXX classes can be implemented once and for all
-- for ReaderSoup.
newtype ReaderSoup_ (record::((Symbol, *) -> *) -> [(Symbol, *)] -> *) ctxs a = ReaderSoup
  { unReaderSoup ::
      ReaderT (record ElField ctxs) IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

-- | The type of 'ReaderSoup_' your application will eat
type ReaderSoup = ReaderSoup_ ARec

-- | A 'ReaderSoup' ready to be eaten
type CookedReaderSoup = ReaderSoup_ Rec


-- * Eating (running) a 'ReaderSoup'

-- | Turns a 'ReaderSoup' into something than is ready to be eaten
cookReaderSoup :: (NatToInt (RLength ctxs))
               => ReaderSoup ctxs a
               -> CookedReaderSoup ctxs a
cookReaderSoup (ReaderSoup (ReaderT act)) =
  ReaderSoup $ ReaderT $ act . toARec

-- | Extracts a ReaderT of the first context so it can be eaten
pickTopping :: (KnownSymbol l)
            => CookedReaderSoup ( (l:::c) : ctxs ) a
            -> ReaderT c (CookedReaderSoup ctxs) a
pickTopping (ReaderSoup (ReaderT actInSoup)) =
  ReaderT $ \ctx1 -> ReaderSoup $
    ReaderT $ \ctxs -> actInSoup $ Field ctx1 :& ctxs

-- | Consumes the first context in the record
eatTopping :: (KnownSymbol l)
           => CookedReaderSoup ( (l:::c) : ctxs ) a
           -> c
           -> CookedReaderSoup ctxs a
eatTopping crs = runReaderT (pickTopping crs)

-- | Once all contexts have been eaten, leaves only the base monad
finishBroth :: (MonadIO m) => CookedReaderSoup '[] a -> m a
finishBroth (ReaderSoup (ReaderT act)) = liftIO $ act RNil

-- | Associates the type-level label to the reader context
type family ContextInSoup (l::Symbol) :: *

type IsInSoup ctxs l =
  ( HasField ARec l ctxs ctxs (ContextInSoup l) (ContextInSoup l) )
  -- , RecElemFCtx ARec ElField )

type IsInCookedSoup ctxs l =
  ( HasField Rec l ctxs ctxs (ContextInSoup l) (ContextInSoup l) )
  -- , RecElemFCtx Rec ElField )


-- * Working in a 'ReaderSoup'

askSoup :: (IsInSoup ctxs l)
        => Label l -> ReaderSoup ctxs (ContextInSoup l)
askSoup l = ReaderSoup $ rvalf l <$> ask

-- | Permits to select only a part of the whole contexts, to locally decide
-- which part of the ReaderSoup will be exposed, and remove ambiguity.
filtering :: (RecSubset ARec ctxs' ctxs (RImage ctxs' ctxs))
          => ReaderSoup ctxs' a
          -> ReaderSoup ctxs a
filtering (ReaderSoup (ReaderT act)) =
  ReaderSoup $ ReaderT $ act . rcast
  -- NOTE: this isn't as fast as 'picking' as it recreates an array, rather than
  -- just a view to the original


-- * Compatibility with existing ReaderT-like monads

-- | Select temporarily one context out of the whole soup to create a
-- MonadReader of that context. 'Chopsticks' behaves exactly like a @ReaderT r
-- IO@ (where r is the ContextInSoup of @l@) but that keeps track of the whole
-- context array.
newtype Chopsticks ctxs (l::Symbol) a = Chopsticks
  { unChopsticks :: ReaderSoup ctxs a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

instance (IsInSoup ctxs l, c ~ ContextInSoup l)
      => MR.MonadReader c (Chopsticks ctxs l) where
  ask = Chopsticks $ askSoup $ fromLabel @l
  local f (Chopsticks (ReaderSoup (ReaderT act))) =
    Chopsticks $ ReaderSoup $ ReaderT $
      act . over (rlensf (fromLabel @l)) f

-- | Brings forth one context of the whole soup, giving a MonadReader instance
-- of just this context. This makes it possible that the same context type
-- occurs several times in the broth, because the Label will disambiguate them.
picking :: (IsInSoup ctxs l)
        => Label l
        -> Chopsticks ctxs l a
        -> ReaderSoup ctxs a
picking _ = unChopsticks

-- | A class for all monads than can be turned into ReaderT r IO
class RIOLike m where
  -- | The reader-like context
  type RIOLikeContext m :: *
  -- | Turn it into an actual ReaderT
  toReader :: m a -> ReaderT (RIOLikeContext m) IO a
  -- | Reconstruct it from an actual ReaderT
  fromReader :: ReaderT (RIOLikeContext m) IO a -> m a

-- | A 'RIOLike' monad that additionnally can generate a default context that
-- can be used to run it
class (RIOLike m) => RunnableRIOLike m where
  createDefaultContext :: proxy m -> IO (RIOLikeContext m)

instance RIOLike (ReaderT r IO) where
  type RIOLikeContext (ReaderT r IO) = r
  toReader = id
  fromReader = id

-- | Converts an action in some ReaderT-of-IO-like monad to 'Chopsticks'. This
-- is for code that cannot cope with any MonadReader and want some specific
-- monad.
withChopsticks :: forall l ctxs m a.
                  (IsInSoup ctxs l, RIOLike m
                  ,RIOLikeContext m ~ ContextInSoup l)
               => m a
               -> Chopsticks ctxs l a
withChopsticks act = Chopsticks $ ReaderSoup $ ReaderT $
  runReaderT (toReader act) . rvalf (fromLabel @l)

-- | Like 'picking', but instead of 'Chopsticks' runs any Reader-like monad.
picking' :: (IsInSoup ctxs l, RIOLike m
            ,RIOLikeContext m ~ ContextInSoup l)
         => Label l
         -> m a
         -> ReaderSoup ctxs a
picking' lbl = picking lbl . withChopsticks

-- class (KnownSymbol (NameInSoup m)) => MonadInSoup m where
--   type NameInSoup m :: Symbol
--   type CtxInSoup m :: *
    
  -- liftInSoup :: () => m a -> ReaderSoup cts a
  -- contextField :: ElField (  )
  -- startContext :: ReaderSoupM ()
  -- runInIO :: m a -> IO a
  -- interrupt :: m a -> IO
  -- rewrapIO  :: IO (a, CtxM m) -> m a

