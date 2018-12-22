{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC "-fno-warn-orphans" #-}

module Control.Monad.ReaderSoup.AWS
  ( Credentials(..)
  , useAWS
  ) where

import           Control.Exception.Safe
import           Control.Monad.Reader
import           Control.Monad.ReaderSoup
import           Control.Monad.ReaderSoup.Resource ()
import           Control.Monad.Trans.AWS
import           Network.AWS


type instance ContextFromName "aws" = Env

instance SoupContext Env AWST where
  toReaderT act = ReaderT $ \env -> runAWST env act
  fromReaderT (ReaderT act) = ask >>= lift . act

useAWS :: (MonadIO m, MonadCatch m) => Credentials -> ContextRunner AWST m
useAWS creds = ContextRunner $ \act -> do
  env <- newEnv creds
  runAWST env act

instance (IsInSoup ctxs "aws", IsInSoup ctxs "resource") => MonadAWS (ReaderSoup ctxs) where
  liftAWS act =
    scooping #aws $
      hoist (picking #resource) act
