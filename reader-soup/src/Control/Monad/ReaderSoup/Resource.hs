{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC "-fno-warn-orphans" #-}

module Control.Monad.ReaderSoup.Resource where

import           Control.Monad.ReaderSoup
import           Control.Monad.Trans.Resource

type instance ContextFromName "resource" = InternalState

instance SoupContext InternalState ResourceT where
  toReaderT act = ReaderT $ runInternalState act
  fromReaderT (ReaderT act) = withInternalState act

data UseResource (m :: * -> *) = UseResource

instance (MonadUnliftIO m) => RunnableTransformer (UseResource m) ResourceT m where
  runTransformer _ = runResourceT

instance (IsInSoup ctxs "resource") => MonadResource (ReaderSoup ctxs) where
  liftResourceT act = picking #resource act
