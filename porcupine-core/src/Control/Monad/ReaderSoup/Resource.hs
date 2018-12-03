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

type instance ContextInSoup "resource" = InternalState

instance RIOLike (ResourceT IO) where
  type RIOLikeContext (ResourceT IO) = InternalState
  toReader act = ReaderT $ runInternalState act
  fromReader (ReaderT act) = withInternalState act

instance RunnableRIOLike (ResourceT IO) where
  createDefaultContext _ = createInternalState

instance (IsInSoup ctxs "resource") => MonadResource (ReaderSoup ctxs) where
  liftResourceT = picking' #resource
