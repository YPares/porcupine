{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC "-fno-warn-orphans" #-}

module Control.Monad.ReaderSoup.AWS
  ( Credentials(..)
  , Region(..)
  , useAWS
  , useAWSRegion
  ) where

import           Control.Exception.Safe
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.ReaderSoup
import           Control.Monad.ReaderSoup.Resource ()
import           Control.Monad.Trans.AWS
import           Katip
import           Network.AWS
import           Network.AWS.Auth                  (AuthError)


type instance ContextFromName "aws" = Env

instance SoupContext Env AWST where
  toReaderT act = ReaderT $ \env -> runAWST env act
  fromReaderT (ReaderT act) = ask >>= lift . act

-- | Tries to get the credentials, but uses dummy credentials if they haven't
-- been found.
getEnv :: (MonadCatch m, KatipContext m) => Credentials -> m Env
getEnv creds = katipAddNamespace "awsContext" $ do
  env <- try $ newEnv creds
  case env of
    Right x -> return x
    Left (e :: AuthError) -> do
      logFM DebugS $ logStr $
        "Using dummy credentials, because there was an error when trying to get the credentials: "
        ++ displayException e
      newEnv (FromKeys "foo" "bar")

-- | See 'Credentials' documentation to know how to
useAWS :: (MonadIO m, MonadCatch m, KatipContext m) => Credentials -> ContextRunner AWST m
useAWS creds = ContextRunner $ \act -> do
  env <- getEnv creds
  runAWST env act

-- | Like 'useAWS', but you set the default 'Region'
useAWSRegion :: (MonadIO m, MonadCatch m, KatipContext m) => Credentials -> Region -> ContextRunner AWST m
useAWSRegion creds region = ContextRunner $ \act -> do
  env <- getEnv creds
  let env' = env & envRegion .~ region
  runAWST env' act

instance (IsInSoup_ r ctxs "aws", IsInSoup_ r ctxs "resource") => MonadAWS (ReaderSoup_ r ctxs) where
  liftAWS act =
    scooping #aws $
      hoist (picking #resource) act
