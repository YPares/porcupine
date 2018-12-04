{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.ReaderSoup.Resource where

import Control.Monad.ReaderSoup
import Control.Monad.Trans.Resource

type instance ContextFromName "resource" = InternalState

instance (MonadUnliftIO m) => SoupContext InternalState m where
  type CtxPrefMonadT InternalState = ResourceT
  type CtxConstructorArgs InternalState = ()
  toReaderT act = ReaderT $ runInternalState act
  fromReaderT (ReaderT act) = withInternalState act
  runPrefMonadT _ _ = runResourceT

instance (MonadUnliftIO m) => BracketedContext InternalState m where
  createCtx _ _ = createInternalState
  closeCtx = closeInternalState

instance (IsInSoup ctxs "resource") => MonadResource (ReaderSoup ctxs) where
  liftResourceT act = inPrefMonad #resource (const act)
