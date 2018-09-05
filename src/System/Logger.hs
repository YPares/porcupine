{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module System.Logger
  ( LoggerScribeParams(..)
  , defaultLoggerScribeParams
  , log
  , runLogger
  ) where

import           Control.Monad.Catch    (MonadMask, bracket)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Katip
import           Prelude                hiding (log)
import           System.IO              (stdout)

-- NOTE: Fields shouldn't be Int, they should be the relevant Katip's types
data LoggerScribeParams = LoggerScribeParams
  { loggerSeverityThreshold :: Int
  , loggerVerbosity         :: Int
  }

defaultLoggerScribeParams :: LoggerScribeParams
defaultLoggerScribeParams = LoggerScribeParams 0 0

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

log :: KatipContext m => LogStr -> m ()
log txt = $(logTM) InfoS txt
