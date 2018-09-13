{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module System.TaskPipeline.Logger
  ( LoggerScribeParams(..)
  , Severity(..)
  , Verbosity(..)
  , defaultLoggerScribeParams
  , log
  , runLogger
  , addContextToTask
  , addNamespaceToTask
  ) where

import           Control.Monad.Catch     (MonadMask, bracket)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Katip
import           System.IO               (stdout)
import           System.TaskPipeline.ATask (ATask(..))


-- | Scribe parameters for Logger. Define a severity threshold and a verbosity level.
data LoggerScribeParams = LoggerScribeParams
  { loggerSeverityThreshold :: Severity
  , loggerVerbosity         :: Verbosity
  }

-- | Default LoggerScribeParams shows log message from Info level, with maximum verbosity.
defaultLoggerScribeParams :: LoggerScribeParams
defaultLoggerScribeParams = LoggerScribeParams InfoS V3

-- | Starts a logger.
runLogger
  :: (MonadMask m, MonadIO m)
  => LoggerScribeParams
  -> KatipContextT m a
  -> m a
runLogger _ x = do
    handleScribe <- liftIO $ mkHandleScribe ColorIfTerminal stdout InfoS V0
    let mkLogEnv = liftIO $
            registerScribe "stdout" handleScribe defaultScribeSettings
                =<< initLogEnv "jinko" "devel"
    bracket mkLogEnv (liftIO . closeScribes) $ \le ->
        runKatipContextT le () "main" $ x

-- -- | 'Loc'-tagged logging when using template-haskell.
-- --
-- -- @$(log) InfoS "Hello world"@
-- log :: ExpQ
-- log = logTM

-- | Adds some context that will be used at logging time. See 'katipAddContext'
addContextToTask
  :: (KatipContext m, LogItem i)
  => i              -- ^ The context
  -> ATask m n a b  -- ^ The task to wrap
  -> ATask m n a b
addContextToTask item (ATask tree fn) =
  ATask tree $ katipAddContext item . fn

-- | Adds a namespace to the task. See 'katipAddNamespace'
addNamespaceToTask 
  :: (KatipContext m)
  => Namespace     -- ^ The namespace. (Is IsString instance)
  -> ATask m n a b -- ^ The task to wrap
  -> ATask m n a b
addNamespaceToTask ns (ATask tree fn) =
  ATask tree $ katipAddNamespace ns . fn
