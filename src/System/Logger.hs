{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module System.Logger
  ( LoggerScribeParams(..)
  , Severity(..)
  , Verbosity(..)
  , defaultLoggerScribeParams
  , log
  , runLogger
  ) where

import           Control.Monad.Catch     (MonadMask, bracket)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Language.Haskell.TH.Lib (ExpQ)
import           Katip
import           Prelude                 hiding (log)
import           System.IO               (stdout)

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

-- | 'Loc'-tagged logging when using template-haskell.
--
-- @$(log) InfoS "Hello world"@
log :: ExpQ
log = logTM
