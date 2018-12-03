{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}

module Control.Monad.ReaderSoup.Resource where

import Control.Monad.ReaderSoup
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource

type instance ContextFromName "resource" = InternalState

instance SoupContext InternalState where
  type CtxMonad InternalState = ResourceT IO
  data CtxConstructorArgs InternalState = UseResourceT  -- no args needed
  toReader act = ReaderT $ runInternalState act
  fromReader (ReaderT act) = withInternalState act
  runCtxMonad _ = runResourceT

instance BracketedContext InternalState where
  createCtx _ = createInternalState
  closeCtx = closeInternalState

instance (IsInSoup ctxs "resource") => MonadResource (ReaderSoup ctxs) where
  liftResourceT act = picking' #resource (const act)
