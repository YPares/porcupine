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

import           Control.Monad.Base
import           Control.Monad.ReaderSoup
import           Control.Monad.Trans
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.Resource.Internal (ResourceT (..))


type instance ContextFromName "resource" = InternalState

instance SoupContext InternalState ResourceT where
  toReaderT act = ReaderT $ runInternalState act
  fromReaderT (ReaderT act) = withInternalState act

useResource :: (MonadUnliftIO m) => ContextRunner ResourceT m
useResource = ContextRunner runResourceT

instance (IsInSoup ctxs "resource") => MonadResource (ReaderSoup ctxs) where
  liftResourceT act = picking #resource act


-- These instances have been removed from resourcet in version 1.2.0
instance MonadBase IO (ResourceT IO) where
    liftBase = lift . liftBase
instance MonadBaseControl IO (ResourceT IO) where
     type StM (ResourceT IO) a = StM IO a
     liftBaseWith f = ResourceT $ \reader' ->
         liftBaseWith $ \runInBase ->
             f $ runInBase . (\(ResourceT r) -> r reader'  )
     restoreM = ResourceT . const . restoreM
