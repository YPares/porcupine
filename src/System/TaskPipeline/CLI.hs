{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TupleSections             #-}
{-# OPTIONS_GHC -Wall          #-}

module System.TaskPipeline.CLI
  ( module System.TaskPipeline.ConfigurationReader
  , PipelineCommand(..)
  , PipelineConfigMethod(..)
  , LocTreeLayout(..)
  , PostParsingAction(..)
  , cliYamlParser
  , execCliParser
  , withCliParser
  , pipelineCliParser

  , pipelineConfigMethodDefRoot
  , tryGetConfigFileOnCLI
  ) where

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Char                               (toLower)
import qualified Data.HashMap.Lazy                       as HashMap
import           Data.Locations
import qualified Data.Text                               as T
import qualified Data.Text.Encoding                      as T
import qualified Data.Yaml                               as Y
import           Katip
import           Options.Applicative
import           System.Directory
import           System.Environment                      (getArgs, withArgs)
import           System.TaskPipeline.ConfigurationReader
import           System.TaskPipeline.Logger


-- | How to print a 'LocationTree' or its mappings on stdout
data LocTreeLayout = NoMappings | FullMappings

data PipelineCommand r where
  RunPipeline :: PipelineCommand r
  ShowLocTree :: (Monoid r) => LocTreeLayout -> PipelineCommand r

-- | Tells whether and how command-line options should be used. @r@ is the
-- result of the Pipeline, which must be a Monoid in case the CLI runs because
-- the user might ask for simply saving the config file.
--
-- NoConfig: No CLI, no Yaml file are used, in which case the root directory
-- should be specified
--
-- FullConfig: We require the config file path, default 'LocationTree' root mapping, and a
-- way to override ModelOpts arguments from CLI and config file.
data PipelineConfigMethod r where
  NoConfig :: Loc -> PipelineConfigMethod r
  FullConfig :: (Monoid r) => String -> Loc -> PipelineConfigMethod r

-- | Set the default Loc that should be bound to the top of the 'LocationTree'
pipelineConfigMethodDefRoot :: Traversal' (PipelineConfigMethod r) Loc
pipelineConfigMethodDefRoot f (FullConfig s r) = FullConfig s <$> f r
pipelineConfigMethodDefRoot f (NoConfig r    ) = NoConfig <$> f r

-- | Runs the new ModelCLI unless a Yaml or Json config file is given on the
-- command line
withCliParser
  :: (Monoid r)
  => String
  -> String
  -> (Maybe FilePath
      -> IO (Parser (Maybe (a, cmd), LoggerScribeParams, [PostParsingAction])))
  -> (a -> cmd -> LoggerScribeParams -> [(Severity, LogStr)] -> IO r)
  -> IO r
withCliParser progName progDesc_ cliParser f = do
  (mbArgs, lsp, actions) <- tryGetConfigFileOnCLI $ \yamlFile ->
    cliParser yamlFile >>= execCliParser progName progDesc_
  logItems <- mapM processAction actions
  case mbArgs of
    Just (cfg, cmd) -> f cfg cmd lsp logItems
    Nothing         -> return mempty
 where
   processAction (PostParsingLog s l) = return (s, l)
   processAction (PostParsingWrite yamlFile cfg) = do
     Y.encodeFile yamlFile cfg
     return (NoticeS, logStr $ "Wrote file '" ++ yamlFile ++ "'")

-- | Creates a command line parser that will return an action returning the
-- configuration and the chosen subcommand or Nothing if the user simply asked
-- to save some overrides and that the program should stop. It _does not_ mean
-- that an error has occured, just that the program should not continue.
cliYamlParser
  :: (ToJSON cfg)
  => String                   -- ^ The program name
  -> FilePath                 -- ^ Configuration file
  -> cfg                      -- ^ Default configuration
  -> ConfigurationReader cfg overrides
  -> [(Parser cmd, String, String)]  -- ^ [(Parser cmd, Command repr, Command help string)]
  -> cmd                      -- ^ Default command
  -> IO (Parser (Maybe (cfg, cmd), LoggerScribeParams, [PostParsingAction]))
cliYamlParser progName configFile defCfg inputParsing cmds defCmd = do
  yamlFound <- doesFileExist configFile
  mcfg <- if yamlFound
    then decodeFile configFile else return Nothing
  return $ pureCliParser progName mcfg configFile defCfg inputParsing cmds defCmd
  where
    decodeFile f = do
      decodeRes <- Y.decodeFileEither f
      pure $ case decodeRes of
        Right x -> Just x
        Left _  -> Nothing

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
  -> Maybe Y.Value            -- ^ Config read
  -> FilePath                 -- ^ Configuration file
  -> cfg                      -- ^ Default configuration
  -> ConfigurationReader cfg overrides
  -> [(Parser cmd, String, String)]  -- ^ [(Parser cmd, Command repr, Command help string)]
  -> cmd                      -- ^ Default command
  -> Parser (Maybe (cfg, cmd), LoggerScribeParams, [PostParsingAction])
                              -- ^ (Config and command, actions to run to
                              -- override the yaml file)
pureCliParser progName mcfg configFile defCfg cfgCLIParsing cmds defCmd =
  subparser
  ( command "write-config-template"
    (info
      (pure (Nothing, defaultLoggerScribeParams, [PostParsingWrite configFile defCfg]))
      (progDesc $ "Write a default configuration file in " <> configFile)))
  <|>
  handleOptions progName configFile mcfg defCfg cliOverriding <$>
   (subparser
    (command "save"
          (info (pure Nothing)
                (progDesc $ "Just save the command line overrides in the " <> configFile))
     <> foldMap
      (\(cmdParser, cmdShown, cmdInfo) ->
        command cmdShown
                (info (Just . (,cmdShown) <$> cmdParser)
                      (progDesc cmdInfo)))
      cmds)
    <|> pure (Just (defCmd, ""))) <*>
   (switch ( long "save"
           <> short 's'
           <> help ("Save overrides in the " <> configFile <> " before running.") )) <*>
   overridesParser cliOverriding
  where cliOverriding = addScribeParamsParsing cfgCLIParsing

-- | Some action to be carried out after the parser is done. Writing the config
-- file is done here, as is the logging of config.
data PostParsingAction
  = PostParsingLog Severity LogStr  -- ^ Log a message
  | forall a. (ToJSON a) => PostParsingWrite String a  -- ^ Write to a file and
                                                       -- log a message about it

handleOptions
  :: forall cfg cmd overrides.
     (ToJSON cfg)
  => String -- ^ Program name
  -> FilePath -- ^ Config file
  -> Maybe Y.Value -- ^ Config body. This is the JSON content of the config file
  -> cfg           -- ^ The default configuration
  -> ConfigurationReader (LoggerScribeParams, cfg) (LoggerScribeParams, overrides)
  -> Maybe (cmd, String) -- ^ Command to run (and a name/description for it)
  -> Bool -- ^ Whether to save the overrides
  -> (LoggerScribeParams, overrides) -- ^ overrides
  -> (Maybe (cfg, cmd), LoggerScribeParams, [PostParsingAction])
                          -- ^ (Config and command, actions to run to override
                          -- the yaml file)
handleOptions progName _ Nothing _ _ Nothing _ _ = error $
  "No config found and nothing to save. Please run `" ++ progName ++ " write-config-template' first."
handleOptions progName configFile mbCfg defCfg cliOverriding mbCmd saveOverridesAlong overrides =
  let defaultCfg = toJSON defCfg
      (cfgWarnings, cfg) = case mbCfg of
        Just c -> mergeWithDefault [] defaultCfg c
        Nothing -> ([PostParsingLog NoticeS $ logStr $
                      configFile ++ " is not found. Treated as empty."]
                   ,defaultCfg)
      (overrideWarnings, mbScribeParamsAndCfgOverriden) =
        overrideCfgFromYamlFile cliOverriding cfg overrides
      allWarnings = cfgWarnings ++ map (PostParsingLog WarningS . logStr) overrideWarnings
  in case mbScribeParamsAndCfgOverriden of
    Right (lsp, cfgOverriden) ->
      case mbCmd of
        Nothing -> (Nothing, lsp, allWarnings ++
                                  [PostParsingWrite configFile cfgOverriden])
        Just (cmd, cmdShown) ->
            let actions =
                  allWarnings ++
                  (if saveOverridesAlong
                     then [PostParsingWrite configFile cfgOverriden]
                     else []) ++
                  [PostParsingLog DebugS $ logStr $ "Running `" <> T.pack progName
                      <> " " <> T.pack cmdShown <> "' with the following config:\n"
                      <> T.decodeUtf8 (Y.encode cfgOverriden)]
            in (Just (cfgOverriden, cmd), lsp, actions)
    Left err -> dispErr err
  where
    dispErr err = error $
      (if nullOverrides cliOverriding overrides
       then "C"
       else "Overriden c") ++ "onfig from " <> configFile <> " is not valid:\n  " ++ err

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

-- | Tries to read a yaml filepath on CLI, then a JSON path, then command line
-- args as expected by the @callParser@ argument. If a filepath is found, this
-- can be an S3 location or a local filepath.
tryGetConfigFileOnCLI
  :: (Maybe FilePath -> IO b)  -- If a filepath has been read as first argument
  -> IO b
tryGetConfigFileOnCLI callParser = do
  cliArgs <- liftIO getArgs
  case cliArgs of
    [] -> callParser Nothing
    filename : rest ->  -- If the first arg is a yaml file path, we extract it
      case map toLower (reverse filename) of
        'l':'m':'a':'y':'.':_ -> goCmdArgs filename rest
        'l':'m':'y':'.':_     -> goCmdArgs filename rest
        _                     -> callParser Nothing
  where
    goCmdArgs fp rest =
      withArgs rest $
        callParser $ Just fp

parseShowLocTree :: (Monoid r) => Parser (PipelineCommand r)
parseShowLocTree = ShowLocTree <$>
  (   (flag' FullMappings
        (long "mappings"
         <> help "Show mappings of all nodes"))
  <|> pure NoMappings
  )

pipelineCliParser
  :: (Monoid r, ToJSON cfg)
  => (cfg -> ConfigurationReader cfg overrides)
  -> String
  -> FilePath
  -> cfg
  -> IO (Parser (Maybe (cfg, PipelineCommand r), LoggerScribeParams, [PostParsingAction]))
pipelineCliParser getCliOverriding progName configFile defCfg =
  cliYamlParser progName configFile defCfg (getCliOverriding defCfg)
  [(pure RunPipeline, "run", "Run the pipeline")
  ,(parseShowLocTree, "show-locations", "Show the location tree of the pipeline")]
  RunPipeline
