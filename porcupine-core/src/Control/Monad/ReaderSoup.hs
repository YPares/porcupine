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
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Control.Monad.ReaderSoup
  ( module Control.Monad.Trans.Reader
  , MonadReader(..)
  , ReaderSoup_(..)
  , ReaderSoup
  , CookedReaderSoup
  , ContextFromName
  , IsInSoup
  , Chopsticks(..)
  , SoupContext(..)
  , BracketedContext(..)
  , cookReaderSoup
  , pickTopping
  , eatTopping
  , finishBroth
  , askSoup
  , filtering
  , picking
  , withChopsticks
  , rioToChopsticks
  , picking'
  ) where

import Control.Lens (over)
import Control.Monad.Reader.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Reader hiding (ask, local, reader)
import Data.Vinyl hiding (record)
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
type family ContextFromName (l::Symbol) :: *

type IsInSoup ctxs l =
  ( HasField ARec l ctxs ctxs (ContextFromName l) (ContextFromName l) )
  -- , RecElemFCtx ARec ElField )

type IsInCookedSoup ctxs l =
  ( HasField Rec l ctxs ctxs (ContextFromName l) (ContextFromName l) )
  -- , RecElemFCtx Rec ElField )


-- * Working in a 'ReaderSoup'

askSoup :: (IsInSoup ctxs l)
        => Label l -> ReaderSoup ctxs (ContextFromName l)
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
-- IO@ (where r is the ContextFromName of @l@) but that keeps track of the whole
-- context array.
newtype Chopsticks ctxs (l::Symbol) a = Chopsticks
  { unChopsticks :: ReaderSoup ctxs a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

instance (IsInSoup ctxs l, c ~ ContextFromName l)
      => MonadReader c (Chopsticks ctxs l) where
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

-- | If you have a code that cannot cope with any MonadReader but explicitly
-- wants a ReaderT
rioToChopsticks :: forall l ctxs a. (IsInSoup ctxs l)
                => ReaderT (ContextFromName l) IO a -> Chopsticks ctxs l a
rioToChopsticks (ReaderT act) = Chopsticks $ ReaderSoup $ ReaderT $
  act . rvalf (fromLabel @l)


-- | A class for the contexts that have an associated monad than can be turned
-- into a ReaderT of this context
class SoupContext c where
  -- | The parameters to construct that context
  data CtxConstructorArgs c :: *
  -- | The reader-like monad using that context
  type CtxMonad c :: * -> *
  -- | Turn this monad into an actual ReaderT
  toReader :: CtxMonad c a -> ReaderT c IO a
  -- | Reconstruct this monad from an actual ReaderT
  fromReader :: ReaderT c IO a -> CtxMonad c a
  -- | Run the CtxMonad
  runCtxMonad :: CtxConstructorArgs c -> CtxMonad c a -> IO a
  default runCtxMonad
    :: (BracketedContext c)
    => CtxConstructorArgs c -> CtxMonad c a -> IO a
  runCtxMonad args act = do
    ctx <- createCtx args
    r <- runReaderT (toReader act) ctx
    closeCtx ctx
    return r

-- | The context is created and closed
class (SoupContext c) => BracketedContext c where
  createCtx :: CtxConstructorArgs c -> IO c
  closeCtx :: c -> IO ()
  closeCtx _ = return ()

-- | Converts an action in some ReaderT-of-IO-like monad to 'Chopsticks', this
-- monad being determined by @c@. This is for code that cannot cope with any
-- MonadReader and want some specific monad.
withChopsticks :: forall l ctxs c a.
                  (IsInSoup ctxs l, SoupContext c
                  ,c ~ ContextFromName l, KnownSymbol l)
               => ((forall x. Chopsticks ctxs l x -> CtxMonad c x) -> CtxMonad c a)
               -> Chopsticks ctxs l a
withChopsticks act = Chopsticks $ ReaderSoup $ ReaderT $ \record ->
  let
    lbl = fromLabel @l
    backwards :: forall x. Chopsticks ctxs l x -> CtxMonad c x
    backwards (Chopsticks (ReaderSoup (ReaderT act'))) =
      fromReader $ ReaderT $ \v -> act' $ rputf lbl v record
  in runReaderT (toReader $ act backwards) $ rvalf lbl record

-- | Like 'picking', but instead of 'Chopsticks' runs some Reader-like monad.
picking' :: (IsInSoup ctxs l, SoupContext c
            ,c ~ ContextFromName l, KnownSymbol l)
         => Label l
         -> ((forall x. ReaderSoup ctxs x -> CtxMonad c x) -> CtxMonad c a)
         -> ReaderSoup ctxs a
picking' lbl f = picking lbl $ withChopsticks $
  \convert -> f (convert . Chopsticks)
