{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module System.TaskPipeline.Logger
  ( LoggerScribeParams(..)
  , LoggerFormat(..)
  , Severity(..)
  , Verbosity(..)
  , defaultLoggerScribeParams
  , log
  , runLogger
  ) where

import           Control.Monad.Catch      (MonadMask, bracket)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Aeson
import           Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder)
import           Data.Aeson.Text          (encodeToTextBuilder)
import qualified Data.HashMap.Strict      as HM
import           Data.String
import           Data.Text                (Text)
import           Data.Text.Lazy.Builder   hiding (fromString)
import           Katip
import           Katip.Core
import           System.IO                (stdout)


-- | Switch between the different type of formatters for the log
data LoggerFormat
  = PrettyLog  -- ^ Just shows the log messages, colored, with namespace and
               -- pretty-prints . For human consumption.
  | CompactLog  -- ^ Like pretty, but prints JSON context just on one line
  | JSONLog  -- ^ JSON-formatted log, from katip
  | BracketLog  -- ^ Regular bracket log, from katip
  deriving (Eq, Show)

-- | Scribe parameters for Logger. Define a severity threshold and a verbosity level.
data LoggerScribeParams = LoggerScribeParams
  { loggerSeverityThreshold :: Severity
  , loggerVerbosity         :: Verbosity
  , loggerFormat            :: LoggerFormat
  }
  deriving (Eq, Show)

-- | Default LoggerScribeParams shows log message from Debug level, with maximum verbosity.
defaultLoggerScribeParams :: LoggerScribeParams
defaultLoggerScribeParams = LoggerScribeParams DebugS V3 PrettyLog

-- | Starts a logger.
runLogger
  :: (MonadMask m, MonadIO m)
  => String
  -> LoggerScribeParams
  -> KatipContextT m a
  -> m a
runLogger progName (LoggerScribeParams sev verb logFmt) x = do
    let logFmt' :: LogItem t => ItemFormatter t
        logFmt' = case logFmt of
          PrettyLog  -> prettyFormat True
          CompactLog -> prettyFormat False
          BracketLog -> bracketFormat
          JSONLog    -> jsonFormat
    handleScribe <- liftIO $
      mkHandleScribeWithFormatter logFmt' ColorIfTerminal stdout sev verb
    let mkLogEnv = liftIO $
          registerScribe "stdout" handleScribe defaultScribeSettings
          =<< initLogEnv (fromString progName) "devel"
    bracket mkLogEnv (liftIO . closeScribes) $ \le ->
        runKatipContextT le () "main" $ x

-- | Doesn't log time, host, file location etc. Colors the whole message and
-- displays context AFTER the message.
prettyFormat :: LogItem a => Bool -> ItemFormatter a
prettyFormat usePrettyJSON withColor verb Item{..} =
    colorize withColor "40" (mconcat $ map fromText $ intercalateNs _itemNamespace) <>
    fromText " " <>
    colorBySeverity' withColor _itemSeverity (mbSeverity <> unLogStr _itemMessage) <>
    colorize withColor "2" ctx
  where
    ctx = case toJSON $ payloadObject verb _itemPayload of
      Object hm | HM.null hm -> mempty
      c -> if usePrettyJSON
        then fromText "\n" <> encodePrettyToTextBuilder c
        else fromText " " <> encodeToTextBuilder c
    -- We display severity levels not distinguished by color
    mbSeverity = case _itemSeverity of
      CriticalS  -> fromText "[CRITICAL] "
      AlertS     -> fromText "[ALERT] "
      EmergencyS -> fromText "[EMERGENCY] "
      _          -> mempty

-- | Like 'colorBySeverity' from katip, but works on builders
colorBySeverity' :: Bool -> Severity -> Builder -> Builder
colorBySeverity' withColor severity msg = case severity of
  EmergencyS -> red msg
  AlertS     -> red msg
  CriticalS  -> red msg
  ErrorS     -> red msg
  WarningS   -> yellow msg
  NoticeS    -> bold msg
  DebugS     -> grey msg
  _          -> msg
  where
    bold = colorize withColor "1"
    red = colorize withColor "31"
    yellow = colorize withColor "33"
    grey = colorize withColor "2"

colorize :: Bool -> Text -> Builder -> Builder
colorize withColor c s
  | withColor = fromText "\ESC[" <> fromText c <> fromText "m" <> s <> fromText "\ESC[0m"
  | otherwise = s
