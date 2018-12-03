{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC "-fno-warn-orphans" #-}

module Control.Monad.ReaderSoup.Katip where

import Control.Monad.ReaderSoup
import Katip
import Katip.Monadic

type instance ContextFromName "katip" = KatipContextTState

instance SoupContext KatipContextTState where
  type CtxMonad KatipContextTState = KatipContextT IO
  data CtxConstructorArgs KatipContextTState =
    forall c. (LogItem c) => KatipArgs LogEnv c Namespace
  toReader (KatipContextT act) = act
  fromReader = KatipContextT
  runCtxMonad (KatipArgs e c n) = runKatipContextT e c n

instance (IsInSoup ctxs "katip") => Katip (ReaderSoup ctxs) where
  getLogEnv = picking' #katip (const getLogEnv)
  localLogEnv f act =
    picking' #katip $ \convert ->
                        localLogEnv f (convert act)

instance (IsInSoup ctxs "katip") => KatipContext (ReaderSoup ctxs) where
  getKatipContext = picking' #katip (const getKatipContext)
  localKatipContext f act =
    picking' #katip $ \convert ->
                        localKatipContext f (convert act)
  getKatipNamespace = picking' #katip (const getKatipNamespace)
  localKatipNamespace f act =
    picking' #katip $ \convert ->
                        localKatipNamespace f (convert act)
