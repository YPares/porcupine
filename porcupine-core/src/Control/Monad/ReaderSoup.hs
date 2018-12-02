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
import GHC.TypeLits


newtype ReaderSoup_ (record::((Symbol, *) -> *) -> [(Symbol, *)] -> *) ctxs m a = ReaderSoup
  { unReaderSoup ::
      ReaderT (record ElField ctxs) m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

type ReaderSoup = ReaderSoup_ ARec
type CookingReaderSoup = ReaderSoup_ Rec

pickTopping :: (KnownSymbol l)
            => CookingReaderSoup ( (l:::ctx) : ctxs ) m a
            -> ReaderT ctx (CookingReaderSoup ctxs m) a
pickTopping (ReaderSoup (ReaderT actInSoup)) =
  ReaderT $ \ctx1 -> ReaderSoup $
    ReaderT $ \ctxs -> actInSoup $ Field ctx1 :& ctxs



type IsInSoup record ctxs m =
  ( HasField record (NameInSoup m) ctxs ctxs (CtxInSoup m) (CtxInSoup m)
  , RecElemFCtx record ElField )

class (KnownSymbol (NameInSoup m)) => MonadInSoup m where
  type NameInSoup m :: Symbol
  type CtxInSoup m :: *

  -- createDefaultContext :: IO (CtxInSoup m)
    
  -- liftInSoup :: () => m a -> ReaderSoup cts a
  -- contextField :: ElField (  )
  -- startContext :: ReaderSoupM ()
  -- runInIO :: m a -> IO a
  -- interrupt :: m a -> IO
  -- rewrapIO  :: IO (a, CtxM m) -> m a

