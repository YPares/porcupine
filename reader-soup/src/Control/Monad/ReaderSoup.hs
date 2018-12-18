{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Control.Monad.ReaderSoup
  ( -- * API for running a ReaderSoup
    ReaderSoup_(..)
  , IsInSoup
  , ArgsForSoupConsumption(..)
  , ContextRunner(..)
  , Label
  , (=:)
  , (:::)
  , Rec(..)
  , consumeSoup

    -- * API for working in a ReaderSoup and creating instances of SoupContext
  , module Control.Monad.Trans.Reader
  , hoist
  , MonadReader(..)
  , ReaderSoup
  , ContextFromName
  , SoupContext(..)
  , CanBeScoopedIn
  , CanRunSoupContext
  , askSoup
  , filtering
  , picking, scooping, pouring

  -- * Low-level API
  , ElField(..)
  , Spoon(..)
  , CookedReaderSoup
  , cookReaderSoup
  , pickTopping
  , eatTopping
  , finishBroth
  , rioToSpoon, spoonToReaderT
  , dipping
  , withSpoon
  , fromLabel
  ) where

import           Control.Lens                (over)
import           Control.Monad.Base          (MonadBase)
import           Control.Monad.Catch
import           Control.Monad.IO.Unlift
import           Control.Monad.Morph         (hoist)
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Reader  hiding (ask, local, reader)
import           Data.Vinyl                  hiding (record)
import           Data.Vinyl.TypeLevel
import           GHC.OverloadedLabels
import           GHC.TypeLits


-- | Represents a set of Reader-like monads as a one-layer Reader that can grow
-- and host more Readers, in a way that's more generic than creating you own
-- application stack of Reader and implementing a host of MonadXXX classes,
-- because each of these MonadXXX classes can be implemented once and for all
-- for the ReaderSoup type.
newtype ReaderSoup_ (record::((Symbol, *) -> *) -> [(Symbol, *)] -> *) ctxs a = ReaderSoup
  { unReaderSoup ::
      ReaderT (record ElField ctxs) IO a }
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadUnliftIO, MonadBase IO, MonadBaseControl IO
           , MonadCatch, MonadThrow, MonadMask )

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
finishBroth :: CookedReaderSoup '[] a -> IO a
finishBroth (ReaderSoup (ReaderT act)) = act RNil

-- | Associates the type-level label to the reader context
type family ContextFromName (l::Symbol) :: *

type IsInSoup ctxs l =
  ( HasField ARec l ctxs ctxs (ContextFromName l) (ContextFromName l) )
  -- , RecElemFContext ARec ElField )


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
  -- NOTE: this isn't as fast as 'picking_' as it recreates an array, rather than
  -- just a view to the original


-- * Compatibility with existing ReaderT-like monads

-- | Select temporarily one context out of the whole soup to create a
-- MonadReader of that context. 'Spoon' behaves exactly like a @ReaderT r
-- IO@ (where r is the ContextFromName of @l@) but that keeps track of the whole
-- context array.
newtype Spoon ctxs (l::Symbol) a = Spoon
  { unSpoon :: ReaderSoup ctxs a }
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadUnliftIO, MonadBase IO, MonadBaseControl IO
           , MonadCatch, MonadThrow, MonadMask )

instance (IsInSoup ctxs l, c ~ ContextFromName l)
      => MonadReader c (Spoon ctxs l) where
  ask = Spoon $ askSoup $ fromLabel @l
  local f (Spoon (ReaderSoup (ReaderT act))) =
    Spoon $ ReaderSoup $ ReaderT $
      act . over (rlensf (fromLabel @l)) f

-- | Brings forth one context of the whole soup, giving a MonadReader instance
-- of just this context. This makes it possible that the same context type
-- occurs several times in the broth, because the Label will disambiguate them.
dipping :: Label l
        -> Spoon ctxs l a
        -> ReaderSoup ctxs a
dipping _ = unSpoon

-- | If you have a code that cannot cope with any MonadReader but explicitly
-- wants a ReaderT
rioToSpoon :: forall l ctxs a. (IsInSoup ctxs l)
           => ReaderT (ContextFromName l) IO a -> Spoon ctxs l a
rioToSpoon (ReaderT act) = Spoon $ ReaderSoup $ ReaderT $
  act . rvalf (fromLabel @l)

-- | Converting Spoon back to a ReaderT has to happen in the ReaderSoup
-- because we need the global context
spoonToReaderT :: forall l ctxs a. (IsInSoup ctxs l, KnownSymbol l)
               => Spoon ctxs l a -> ReaderT (ContextFromName l) (ReaderSoup ctxs) a
spoonToReaderT (Spoon (ReaderSoup (ReaderT act))) =
  ReaderT $ \v -> ReaderSoup $ ReaderT $ \record ->
    act $ rputf (fromLabel @l) v record

-- | A class for the contexts that have an associated monad transformer that can
-- be turned into a ReaderT of this context, and the type of monad over which
-- they can run.
class SoupContext c t | c -> t where
  -- | Turn this monad trans into an actual ReaderT
  toReaderT :: (Monad m) => t m a -> ReaderT c m a
  -- | Reconstruct this monad trans from an actual ReaderT
  fromReaderT :: (Monad m) => ReaderT c m a -> t m a

type CanBeScoopedIn t ctxs l =
  (IsInSoup ctxs l, KnownSymbol l, SoupContext (ContextFromName l) t)

-- | Converts an action in some ReaderT-like monad to 'Spoon', this
-- monad being determined by @c@. This is for code that cannot cope with any
-- MonadReader and want some specific monad.
withSpoon :: forall l ctxs t a.
             (CanBeScoopedIn t ctxs l)
          => t (ReaderSoup ctxs) a
          -> Spoon ctxs l a
withSpoon act = Spoon $ ReaderSoup $ ReaderT $ \record ->
  runReaderT (unReaderSoup $
               (runReaderT (toReaderT act) $
                           rvalf (fromLabel @l) record))
             record

-- | Like 'dipping', but instead of 'Spoon' runs some preferential Reader-like
-- monad. That permits to reuse some already existing monad from an existing
-- library (ResourceT, KatipContextT, etc.) if you cannot just use a MonadReader
-- instance.
picking :: (CanBeScoopedIn t ctxs l)
        => Label l
        -> t IO a
        -> ReaderSoup ctxs a
picking lbl = dipping lbl . rioToSpoon . toReaderT

-- | Like 'picking', but gives you more context: instead of just running over
-- IO, it makes the monad run over the whole soup (so instances of MonadXXX
-- classes defined over the whole soup can still be used).
scooping :: (CanBeScoopedIn t ctxs l)
         => Label l
         -> t (ReaderSoup ctxs) a
         -> ReaderSoup ctxs a
scooping lbl = dipping lbl . withSpoon

-- | The opposite of 'scooping'.
pouring :: forall l ctxs t a.
           (CanBeScoopedIn t ctxs l)
        => Label l
        -> ReaderSoup ctxs a
        -> t (ReaderSoup ctxs) a
pouring _ act = fromReaderT $ spoonToReaderT (Spoon act :: Spoon ctxs l a)


-- * Running a whole 'ReaderSoup'

-- | Knowing the prefered monad to run some context, gives you a way to override
-- this monad's runner.
newtype ContextRunner t m = ContextRunner
  { runContext :: forall r. t m r -> m r }

class (NatToInt (RLength (ContextsFromArgs args))) => ArgsForSoupConsumption args where
  type ContextsFromArgs args :: [(Symbol, *)]
  consumeSoup_ :: Rec ElField args -> CookedReaderSoup (ContextsFromArgs args) a -> IO a

instance ArgsForSoupConsumption '[] where
  type ContextsFromArgs '[] = '[]
  consumeSoup_ _ = finishBroth

type CanRunSoupContext l t =
  (SoupContext (ContextFromName l) t)

instance ( ArgsForSoupConsumption restArgs
         , m ~ CookedReaderSoup (ContextsFromArgs restArgs)
         , CanRunSoupContext l t )
      => ArgsForSoupConsumption ((l:::ContextRunner t m) : restArgs) where
  type ContextsFromArgs ((l:::ContextRunner t m) : restArgs) =
    (l:::ContextFromName l) : ContextsFromArgs restArgs
  consumeSoup_ (Field args :& restArgs) act =
    consumeSoup_ restArgs $
      runContext args (fromReaderT (pickTopping act))

-- | From the list of the arguments to initialize the contexts, runs the whole
-- 'ReaderSoup'
consumeSoup :: (ArgsForSoupConsumption args)
            => Rec ElField args -> ReaderSoup (ContextsFromArgs args) a -> IO a
consumeSoup args = consumeSoup_ args . cookReaderSoup
