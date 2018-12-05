{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Control.Monad.ReaderSoup.Resource where

import           Control.Monad.ReaderSoup
import           Control.Monad.Trans.Resource

type instance ContextFromName "resource" = InternalState

instance (MonadUnliftIO m) => SoupContext InternalState m where
  type CtxPrefMonadT InternalState = ResourceT
  type CtxConstructorArgs InternalState = ()
  toReaderT act = ReaderT $ runInternalState act
  fromReaderT (ReaderT act) = withInternalState act
  runPrefMonadT _ _ = runResourceT

instance (IsInSoup ctxs "resource") => MonadResource (ReaderSoup ctxs) where
  liftResourceT act = picking #resource act
