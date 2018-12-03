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
