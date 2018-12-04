{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC "-fno-warn-orphans" #-}

module Control.Monad.ReaderSoup.Katip where

import Control.Monad.ReaderSoup
import Katip
import Katip.Monadic

type instance ContextFromName "katip" = KatipContextTState

instance (Monad m) => SoupContext KatipContextTState m where
  type CtxPrefMonadT KatipContextTState = KatipContextT
  type CtxConstructorArgs KatipContextTState = (LogEnv, Namespace)
  toReaderT (KatipContextT act) = act
  fromReaderT = KatipContextT
  runPrefMonadT _ (e,n) = runKatipContextT e () n

instance (IsInSoup ctxs "katip") => Katip (ReaderSoup ctxs) where
  getLogEnv = inPrefMonad #katip (const getLogEnv)
  localLogEnv f act =
    inPrefMonad #katip $ \convert ->
                        localLogEnv f (convert act)

instance (IsInSoup ctxs "katip") => KatipContext (ReaderSoup ctxs) where
  getKatipContext = inPrefMonad #katip (const getKatipContext)
  localKatipContext f act =
    inPrefMonad #katip $ \convert ->
                        localKatipContext f (convert act)
  getKatipNamespace = inPrefMonad #katip (const getKatipNamespace)
  localKatipNamespace f act =
    inPrefMonad #katip $ \convert ->
                        localKatipNamespace f (convert act)
