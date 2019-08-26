{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module contains some helper functions for logging info and throwing
-- errors

module Data.Locations.LogAndErrors
  ( module Control.Exception.Safe
  , KatipContext
  , LogThrow, LogCatch, LogMask
  , TaskRunError(..)
  , logAndThrowM
  , throwWithPrefix
  ) where

import           Control.Exception.Safe
import qualified Data.Text              as T
import           GHC.Stack
import           Katip


-- | An error when running a pipeline of tasks
newtype TaskRunError = TaskRunError String

instance Show TaskRunError where
  show (TaskRunError s) = s

instance Exception TaskRunError where
  displayException (TaskRunError s) = s

getTaskErrorPrefix :: (KatipContext m) => m String
getTaskErrorPrefix = do
  Namespace ns <- getKatipNamespace
  case ns of
    [] -> return ""
    _  -> return $ T.unpack $ T.intercalate "." ns <> ": "

-- | Just an alias for monads that can throw errors and log them
type LogThrow m = (KatipContext m, MonadThrow m, HasCallStack)

-- | Just an alias for monads that can throw,catch errors and log them
type LogCatch m = (KatipContext m, MonadCatch m)

-- | Just an alias for monads that can throw,catch,mask errors and log them
type LogMask m = (KatipContext m, MonadMask m)

-- | A replacement for throwM. Logs an error (using displayException) and throws
logAndThrowM :: (LogThrow m, Exception e) => e -> m a
logAndThrowM exc = do
  logFM ErrorS $ logStr $ displayException exc
  throwM exc

-- | Logs an error and throws a 'TaskRunError'
throwWithPrefix :: (LogThrow m) => String -> m a
throwWithPrefix msg = do
  logFM ErrorS $ logStr msg
  prefix <- getTaskErrorPrefix
  throwM $ TaskRunError $ prefix ++ msg
