{-# LANGUAGE OverloadedStrings #-}

-- | This module contains some helper functions for logging info and throwing
-- errors

module Data.Locations.LogAndErrors
  ( module Control.Monad.Catch
  , KatipContext
  , TaskRunError(..)
  , throwWithPrefix
  ) where

import           Control.Monad.Catch
import qualified Data.Text           as T
import           Katip               hiding (logMsg)


-- | An error when running a pipeline of tasks
newtype TaskRunError = TaskRunError String
  deriving (Show)

instance Exception TaskRunError where
  displayException (TaskRunError s) = s

getTaskErrorPrefix :: (KatipContext m) => m String
getTaskErrorPrefix = do
  Namespace ns <- getKatipNamespace
  case ns of
    [] -> return ""
    _  -> return $ T.unpack $ T.intercalate "." ns <> ": "

-- | Logs an error and throws a 'TaskRunError'
throwWithPrefix :: (KatipContext m, MonadThrow m) => String -> m a
throwWithPrefix msg = do
  logFM ErrorS $ logStr msg
  prefix <- getTaskErrorPrefix
  throwM $ TaskRunError $ prefix ++ msg
