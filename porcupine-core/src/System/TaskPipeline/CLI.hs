{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# OPTIONS_GHC -Wall          #-}

module System.TaskPipeline.CLI
  ( module System.TaskPipeline.ConfigurationReader
  , PipelineCommand(..)
  , PipelineConfigMethod(..)
  , BaseInputConfig(..)
  , PostParsingAction(..)
  , PreRun(..)
  , ConfigFileSource(..)
  , tryReadConfigFileSource
  , cliYamlParser
  , execCliParser
  , withCliParser
  , pipelineCliParser

  , pipelineConfigMethodProgName
  , pipelineConfigMethodDefRoot
  , pipelineConfigMethodConfigFile
  , pipelineConfigMethodDefReturnVal
  , withConfigFileSourceFromCLI
  ) where

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Aeson                              as A
import qualified Data.Aeson.Encode.Pretty                as A
import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Lazy                    as LBS
import           Data.Char                               (toLower)
import qualified Data.HashMap.Lazy                       as HashMap
import           Data.Locations
import           Data.Maybe
import qualified Data.Text                               as T
import qualified Data.Text.Encoding                      as T
import qualified Data.Yaml                               as Y
import           Katip
import           Options.Applicative
import           System.Directory
import           System.Environment                      (getArgs, withArgs)
import           System.IO                               (stdin)
import           System.TaskPipeline.ConfigurationReader
import           System.TaskPipeline.Logger
import           System.TaskPipeline.PorcupineTree       (PhysicalFileNodeShowOpts(..))


-- | The command to parse from the CLI
data PipelineCommand
  = RunPipeline
  | ShowLocTree PhysicalFileNodeShowOpts

-- | Tells whether and how command-line options should be used. @r@ is the type
-- of the result of the PTask that this PipelineConfigMethod will be use for
-- (see @runPipelineTask@). _pipelineConfigMethodDefReturnVal will be used as a
-- return value for the pipeline eg. when the user just wants to write the
-- config template, and not run the pipeline.
data PipelineConfigMethod r
  = NoConfig { _pipelineConfigMethodProgName :: String
             , _pipelineConfigMethodDefRoot  :: Loc }
    -- ^ No CLI and no config file are used, in which case we require a program
    -- name (for logs) and the root directory.
  | ConfigFileOnly { _pipelineConfigMethodProgName   :: String
                   , _pipelineConfigMethodConfigFile :: LocalFilePath
                   , _pipelineConfigMethodDefRoot    :: Loc }
    -- ^ Config file is read and loaded if it exists. No CLI is used.
  | FullConfig { _pipelineConfigMethodProgName     :: String
               , _pipelineConfigMethodConfigFile   :: LocalFilePath
               , _pipelineConfigMethodDefRoot      :: Loc
               , _pipelineConfigMethodDefReturnVal :: r }
    -- ^ Both config file and CLI will be used. We require a name (for the help
    -- page in the CLI), the config file path, the default 'LocationTree' root
    -- mapping, and a default value to return when the CLI is called with a
    -- command other than 'run' (in which case the pipeline will not run).

makeLenses ''PipelineConfigMethod

-- | Runs the new ModelCLI unless a Yaml or Json config file is given on the
-- command line
withCliParser
  :: String
  -> String
  -> Parser (Maybe (a, cmd), LoggerScribeParams, [PostParsingAction])
  -> r
  -> (a -> cmd -> LoggerScribeParams -> PreRun -> IO r)
  -> IO r
withCliParser progName progDesc_ cliParser defRetVal f = do
  (mbArgs, lsp, actions) <- execCliParser progName progDesc_ cliParser
  case mbArgs of
    Just (cfg, cmd) ->
      f cfg cmd lsp $ PreRun $ mapM_ processAction actions
    Nothing         -> runLogger progName lsp $ do
      mapM_ processAction actions
      return defRetVal
 where
   processAction (PostParsingLog s l) = logFM s l
   processAction (PostParsingWrite configFile cfg) = do
     let rawFile = configFile ^. locFilePathAsRawFilePath
     case configFile of
       LocFilePath "-" _ ->
         error "Config was read from stdin, cannot overwrite it"
       LocFilePath _ e | e `elem` ["yaml","yml"] ->
         liftIO $ Y.encodeFile rawFile cfg
       LocFilePath _ "json" ->
         liftIO $ LBS.writeFile rawFile $ A.encodePretty cfg
       _ -> error $ "Config file has unknown format"
     logFM NoticeS $ logStr $ "Wrote file '" ++ rawFile ++ "'"

data ConfigFileSource = YAMLStdin | ConfigFileURL Loc

-- | Tries to read a yaml filepath on CLI, then a JSON path, then command line
-- args as expected by the @callParser@ argument.
withConfigFileSourceFromCLI
  :: (Maybe ConfigFileSource -> IO b)  -- If a filepath has been read as first argument
  -> IO b
withConfigFileSourceFromCLI f = do
  cliArgs <- liftIO getArgs
  case cliArgs of
    [] -> f Nothing
    filename : rest -> do
      case parseURLLikeLoc filename of
        Left _ -> f Nothing
        Right (LocalFile (LocFilePath "-" ext)) ->
          if ext == "" || allowedExt ext
          then withArgs rest $ f $ Just YAMLStdin
          else error $ filename ++ ": Only JSON or YAML config can be read from stdin"
        Right loc | getLocType loc == "" -> f Nothing
                        -- No extension, therefore probably not a filepath
                  | allowedExt (getLocType loc) -> withArgs rest $ f $ Just $ ConfigFileURL loc
                  | otherwise -> error $ filename ++ ": Only JSON or YAML cofiles allowed"
  where
    allowedExt = (`elem` ["yaml","yml","json"]) . map toLower

tryReadConfigFileSource :: (FromJSON cfg)
                        => ConfigFileSource -> (Loc -> IO cfg) -> IO (Maybe cfg)
tryReadConfigFileSource configFileSource ifRemote =
  case configFileSource of
    YAMLStdin ->
      Just <$> (BS.hGetContents stdin >>= Y.decodeThrow)
    ConfigFileURL (LocalFile lfp) -> do
      let p = lfp ^. locFilePathAsRawFilePath
      yamlFound <- doesFileExist p
      if yamlFound
        then Just <$> Y.decodeFileThrow p
        else return Nothing
    ConfigFileURL remoteLoc -> -- If config is remote, it must always be present
                               -- for now
      Just <$> ifRemote remoteLoc

data BaseInputConfig cfg = BaseInputConfig
  { bicSourceFile    :: Maybe LocalFilePath
  , bicLoadedConfig  :: Maybe Y.Value
  , bicDefaultConfig :: cfg
  }

-- | Creates a command line parser that will return an action returning the
-- configuration and the chosen subcommand or Nothing if the user simply asked
-- to save some overrides and that the program should stop. It _does not_ mean
-- that an error has occured, just that the program should not continue.
cliYamlParser
  :: (ToJSON cfg)
  => String                   -- ^ The program name
  -> BaseInputConfig cfg      -- ^ Default configuration
  -> ConfigurationReader cfg overrides
  -> [(Parser cmd, String, String)]  -- ^ [(Parser cmd, Command repr, Command help string)]
  -> cmd                      -- ^ Default command
  -> IO (Parser (Maybe (cfg, cmd), LoggerScribeParams, [PostParsingAction]))
cliYamlParser progName baseInputCfg inputParsing cmds defCmd = do
  return $ pureCliParser progName baseInputCfg inputParsing cmds defCmd

-- | A shortcut to run a parser and defining the program help strings
execCliParser
  :: String
  -> String
  -> Parser a
  -> IO a
execCliParser header_ progDesc_ parser_ = do
  let opts = info (helper <*> parser_)
        ( fullDesc
          <> header header_
          <> progDesc progDesc_ )
  execParser opts

pureCliParser
  :: (ToJSON cfg)
  => String                   -- ^ The program name
  -> BaseInputConfig cfg      -- ^ The base configuration we read
  -> ConfigurationReader cfg overrides
  -> [(Parser cmd, String, String)]  -- ^ [(Parser cmd, Command repr, Command help string)]
  -> cmd                      -- ^ Default command
  -> Parser (Maybe (cfg, cmd), LoggerScribeParams, [PostParsingAction])
                              -- ^ (Config and command, actions to run to
                              -- override the yaml file)
pureCliParser progName baseInputConfig cfgCLIParsing cmds defCmd =
  (case bicSourceFile baseInputConfig of
     Nothing -> empty
     Just f  -> subparser $
       command "write-config-template"
        (info
         (pure (Nothing, maxVerbosityLoggerScribeParams
               ,[PostParsingWrite f (bicDefaultConfig baseInputConfig)]))
         (progDesc $ "Write a default configuration file in " <> (f^.locFilePathAsRawFilePath))))
  <|>
  handleOptions progName baseInputConfig cliOverriding
    <$> ((subparser $
           (case bicSourceFile baseInputConfig of
              Nothing -> mempty
              Just f ->
                command "save" $
                  info (pure Nothing) $
                       progDesc $ "Just save the command line overrides in " <> (f^.locFilePathAsRawFilePath))
           <>
           foldMap
             (\(cmdParser, cmdShown, cmdInfo) ->
                 command cmdShown $
                   info (Just . (,cmdShown) <$> cmdParser) $
                        progDesc cmdInfo)
             cmds)
         <|>
         pure (Just (defCmd, "")))
    <*> (case bicSourceFile baseInputConfig of
           Nothing -> pure False
           Just f  ->
             switch ( long "save"
                   <> short 's'
                   <> help ("Save overrides in the " <> (f^.locFilePathAsRawFilePath) <> " before running.") ))
    <*> overridesParser cliOverriding
  where
    cliOverriding = addScribeParamsParsing cfgCLIParsing

severityShortcuts :: Parser Severity
severityShortcuts =
  numToSeverity <$> liftA2 (-)
    (length <$>
      (many
        (flag' ()
          (  short 'q'
          <> long "quiet"
          <> help "Print only warning (-q) or error (-qq) messages. Cancels out with -v."))))
    (length <$>
      (many
        (flag' ()
          (  long "verbose"
          <> short 'v'
          <> help "Print info (-v) and debug (-vv) messages. Cancels out with -q."))))
  where
    numToSeverity (-1) = InfoS
    numToSeverity 0 = NoticeS
    numToSeverity 1 = WarningS
    numToSeverity 2 = ErrorS
    numToSeverity 3 = CriticalS
    numToSeverity 4 = AlertS
    numToSeverity x | x>0 = EmergencyS
                    | otherwise = DebugS

-- | Parses the CLI options that will be given to Katip's logger scribe
parseScribeParams :: Parser LoggerScribeParams
parseScribeParams = LoggerScribeParams
  <$> ((option (eitherReader severityParser)
          (  long "severity"
          <> help "Control exactly which minimal severity level will be logged (used instead of -q or -v)"))
       <|>
       severityShortcuts)
  <*> (numToVerbosity <$>
       option auto
         (  long "context-verb"
         <> help "A number from 0 to 3 (default: 0). Controls the amount of context to show per log line"
         <> value (0 :: Int)))
  <*> (option (eitherReader loggerFormatParser)
        (  long "log-format"
        <> help "Selects a format for the log: 'pretty' (default, only for human consumption), 'compact' (pretty but more compact), 'json' or 'bracket'"
        <> value PrettyLog))
  where
    severityParser = \case
        "debug" -> Right DebugS
        "info" -> Right InfoS
        "notice" -> Right NoticeS
        "warning" -> Right WarningS
        "error" -> Right ErrorS
        "critical" -> Right CriticalS
        "alert" -> Right AlertS
        "emergency" -> Right EmergencyS
        s -> Left $ s ++ " isn't a valid severity level"
    numToVerbosity 0 = V0
    numToVerbosity 1 = V1
    numToVerbosity 2 = V2
    numToVerbosity _ = V3
    loggerFormatParser "pretty"  = Right PrettyLog
    loggerFormatParser "compact" = Right CompactLog
    loggerFormatParser "json"    = Right JSONLog
    loggerFormatParser "bracket" = Right BracketLog
    loggerFormatParser s         = Left $ s ++ " isn't a valid log format"

-- | Modifies a CLI parsing so it features verbosity and severity flags
addScribeParamsParsing :: ConfigurationReader cfg ovs -> ConfigurationReader (LoggerScribeParams, cfg) (LoggerScribeParams, ovs)
addScribeParamsParsing super = ConfigurationReader
  { overridesParser = (,) <$> parseScribeParams <*> overridesParser super
  , nullOverrides = \(_, ovs) -> nullOverrides super ovs
  , overrideCfgFromYamlFile = \yaml (scribeParams, ovs) ->
      let (warns, res) = overrideCfgFromYamlFile super yaml ovs
      in (warns, (scribeParams,) <$> res)
  }

-- | Some action to be carried out after the parser is done. Writing the config
-- file is done here, as is the logging of config.
data PostParsingAction
  = PostParsingLog Severity LogStr  -- ^ Log a message
  | forall a. (ToJSON a) => PostParsingWrite LocalFilePath a
          -- ^ Write to a file and log a message about it

-- | Wraps the actions to override the config file
newtype PreRun = PreRun {unPreRun :: forall m. (KatipContext m, MonadIO m) => m ()}

handleOptions
  :: forall cfg cmd overrides.
     (ToJSON cfg)
  => String -- ^ Program name
  -> BaseInputConfig cfg
  -> ConfigurationReader (LoggerScribeParams, cfg) (LoggerScribeParams, overrides)
  -> Maybe (cmd, String) -- ^ Command to run (and a name/description for it). If
                         -- Nothing, means we should just save the config
  -> Bool -- ^ Whether to save the overrides
  -> (LoggerScribeParams, overrides) -- ^ overrides
  -> (Maybe (cfg, cmd), LoggerScribeParams, [PostParsingAction])
                          -- ^ (Config and command, actions to run to override
                          -- the yaml file)
handleOptions progName (BaseInputConfig _ Nothing _) _ Nothing _ _ = error $
  "No config found and nothing to save. Please run `" ++ progName ++ " write-config-template' first."
handleOptions progName (BaseInputConfig mbCfgFile mbCfg defCfg) cliOverriding mbCmd saveOverridesAlong overrides =
  let defaultCfg = toJSON defCfg
      (cfgWarnings, cfg) = case mbCfg of
        Just c -> mergeWithDefault [] defaultCfg c
        Nothing -> ([PostParsingLog DebugS $ logStr $
                      "Config file" ++ configFile' ++ " is not found. Treated as empty."]
                   ,defaultCfg)
      (overrideWarnings, mbScribeParamsAndCfgOverriden) =
        overrideCfgFromYamlFile cliOverriding cfg overrides
      allWarnings = cfgWarnings ++ map (PostParsingLog WarningS . logStr) overrideWarnings
  in case mbScribeParamsAndCfgOverriden of
    Right (lsp, cfgOverriden) ->
      case mbCmd of
        Nothing -> (Nothing, lsp, allWarnings ++
                                  [PostParsingWrite (fromJust mbCfgFile) cfgOverriden])
        Just (cmd, cmdShown) ->
            let actions =
                  allWarnings ++
                  (if saveOverridesAlong
                     then [PostParsingWrite (fromJust mbCfgFile) cfgOverriden]
                     else []) ++
                  [PostParsingLog DebugS $ logStr $ "Running `" <> T.pack progName
                      <> " " <> T.pack cmdShown <> "' with the following config:\n"
                      <> T.decodeUtf8 (Y.encode cfgOverriden)]
            in (Just (cfgOverriden, cmd), lsp, actions)
    Left err -> dispErr err
  where
    configFile' = case mbCfgFile of Nothing -> ""
                                    Just f -> " " ++ f ^. locFilePathAsRawFilePath
    dispErr err = error $
      (if nullOverrides cliOverriding overrides
       then "C"
       else "Overriden c") ++ "onfig" <> shownFile <> " is not valid:\n  " ++ err
      where
        shownFile = case mbCfgFile of
          Just f  -> " from " <> show f
          Nothing -> ""

mergeWithDefault :: [T.Text] -> Y.Value -> Y.Value -> ([PostParsingAction], Y.Value)
mergeWithDefault path (Object o1) (Object o2) =
  let newKeys = HashMap.keys $ o2 `HashMap.difference` o1
      warnings = map (\key -> PostParsingLog DebugS $ logStr $
                       "The key '" <> T.intercalate "." (reverse $ key:path) <>
                       "' isn't present in the default configuration. " <>
                       "Please make sure it isn't a typo.")
                     newKeys
      (subWarnings, merged) =
        sequenceA $ HashMap.unionWithKey
                      (\key (_,v1) (_,v2) -> mergeWithDefault (key:path) v1 v2)
                      (fmap pure o1)
                      (fmap pure o2)
  in (warnings ++ subWarnings, Object merged)
mergeWithDefault _ _ v = pure v

parseShowLocTree :: Parser PipelineCommand
parseShowLocTree = fmap ShowLocTree $ PhysicalFileNodeShowOpts
  <$> flag False True
       (long "mappings"
        <> short 'm'
        <> help "Show mappings of virtual files")
  <*> flag True False
       (long "no-serials"
        <> short 'S'
        <> help "Don't show if the virtual file can be used as a source or a sink")
  <*> flag False True
       (long "types"
        <> short 't'
        <> help "Show types written to virtual files")
  <*> flag False True
       (long "accesses"
        <> short 'a'
        <> help "Show how virtual files will be accessed")
  <*> flag True False
       (long "no-extensions"
        <> short 'E'
        <> help "Don't show the possible extensions for physical files")
  <*> option auto
       (long "num-chars"
        <> short 'c'
        <> help "The number of characters to show for the type (default: 60)"
        <> value (60 :: Int))

pipelineCliParser
  :: (ToJSON cfg)
  => (cfg -> ConfigurationReader cfg overrides)
  -> String
  -> BaseInputConfig cfg
  -> IO (Parser (Maybe (cfg, PipelineCommand), LoggerScribeParams, [PostParsingAction]))
pipelineCliParser getCliOverriding progName baseInputConfig =
  cliYamlParser progName baseInputConfig (getCliOverriding $ bicDefaultConfig baseInputConfig)
  [(pure RunPipeline, "run", "Run the pipeline")
  ,(parseShowLocTree, "show-tree", "Show the porcupine tree of the pipeline")]
  RunPipeline
