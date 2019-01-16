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

-- | Use a katip context, parameterized by a 'LogEnv' and a root 'Namespace'
useKatip :: LogEnv -> Namespace -> ContextRunner KatipContextT m
useKatip e n = ContextRunner $ runKatipContextT e () n

instance (IsInSoup_ r ctxs "katip") => Katip (ReaderSoup_ r ctxs) where
  getLogEnv = picking #katip getLogEnv
  localLogEnv f act = scooping #katip $
    localLogEnv f (pouring #katip act)

instance (IsInSoup_ r ctxs "katip") => KatipContext (ReaderSoup_ r ctxs) where
  getKatipContext = picking #katip getKatipContext
  localKatipContext f act = scooping #katip $
    localKatipContext f (pouring #katip act)

  getKatipNamespace = picking #katip getKatipNamespace
  localKatipNamespace f act = scooping #katip $
    localKatipNamespace f (pouring #katip act)
