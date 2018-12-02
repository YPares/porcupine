{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Control.Monad.ReaderSoup where

import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Data.Vinyl
import Data.Vinyl.Derived
import Data.Vinyl.Functor
import Data.Vinyl.TypeLevel
import GHC.TypeLits


-- | Represents a set of Reader-like monads as a one-layer Reader that can grow
-- and host more Readers, in a way that's more generic than creating you own
-- application stack of Reader and implementing a host of MonadXXX classes,
-- because each of these MonadXXX classes can be implemented once and for all
-- for the ReaderSoup type.
newtype ReaderSoup_ (record::((Symbol, *) -> *) -> [(Symbol, *)] -> *) ctxs m a = ReaderSoup
  { unReaderSoup ::
      ReaderT (record ElField ctxs) m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

-- | The type of 'ReaderSoup_' your application will eat
type ReaderSoup = ReaderSoup_ ARec

-- | A 'ReaderSoup' ready to be eaten
type CookedReaderSoup = ReaderSoup_ Rec

-- | Turns a 'ReaderSoup' into something than is ready to be eaten (ran)
cookReaderSoup :: (NatToInt (RLength ctxs))
               => ReaderSoup ctxs m a
               -> CookedReaderSoup ctxs m a
cookReaderSoup (ReaderSoup (ReaderT act)) =
  ReaderSoup $ ReaderT $ act . toARec

-- | Extracts the first context so it can be eaten (ran)
pickTopping :: (KnownSymbol l)
            => CookedReaderSoup ( (l:::ctx) : ctxs ) m a
            -> ReaderT ctx (CookedReaderSoup ctxs m) a
pickTopping (ReaderSoup (ReaderT actInSoup)) =
  ReaderT $ \ctx1 -> ReaderSoup $
    ReaderT $ \ctxs -> actInSoup $ Field ctx1 :& ctxs

-- | Once all ingredients have been eaten, leaves only the bowl (the base monad)
finishBroth :: CookedReaderSoup '[] m a -> m a
finishBroth (ReaderSoup (ReaderT act)) = act RNil


-- type IsInSoup record ctxs m =
--   ( HasField record (NameInSoup m) ctxs ctxs (CtxInSoup m) (CtxInSoup m)
--   , RecElemFCtx record ElField )

-- class (KnownSymbol (NameInSoup m)) => MonadInSoup m where
--   type NameInSoup m :: Symbol
--   type CtxInSoup m :: *
    
  -- liftInSoup :: () => m a -> ReaderSoup cts a
  -- contextField :: ElField (  )
  -- startContext :: ReaderSoupM ()
  -- runInIO :: m a -> IO a
  -- interrupt :: m a -> IO
  -- rewrapIO  :: IO (a, CtxM m) -> m a

