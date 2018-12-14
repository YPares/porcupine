{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC "-fno-warn-orphans" #-}

module Control.Monad.ReaderSoup.Katip where

import           Control.Monad.ReaderSoup
import           Katip
import           Katip.Monadic

type instance ContextFromName "katip" = KatipContextTState

instance SoupContext KatipContextTState KatipContextT where
  toReaderT (KatipContextT act) = act
  fromReaderT = KatipContextT

-- | The usual parameter types to run a katip context
data UseKatip (m :: * -> *) = UseKatip LogEnv Namespace

instance RunnableTransformer (UseKatip m) KatipContextT m where
  runTransformer (UseKatip e n) = runKatipContextT e () n

instance (IsInSoup ctxs "katip") => Katip (ReaderSoup ctxs) where
  getLogEnv = picking #katip getLogEnv
  localLogEnv f act = scooping #katip $
    localLogEnv f (pouring #katip act)

instance (IsInSoup ctxs "katip") => KatipContext (ReaderSoup ctxs) where
  getKatipContext = picking #katip getKatipContext
  localKatipContext f act = scooping #katip $
    localKatipContext f (pouring #katip act)

  getKatipNamespace = picking #katip getKatipNamespace
  localKatipNamespace f act = scooping #katip $
    localKatipNamespace f (pouring #katip act)
