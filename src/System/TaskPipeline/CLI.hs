{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -Wall          #-}

module System.TaskPipeline.CLI
  ( module System.TaskPipeline.CLI.Overriding
  , PipelineCommand(..)
  , PipelineConfigMethod(..)
  , LocTreeLayout(..)
  , cliYamlParser
  , execCliParser
  , withCliParser
  , pipelineCliParser

  , pipelineConfigMethodDefRoot
  , tryGetConfigFileOnCLI
  ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Char                          (toLower)
import qualified Data.HashMap.Lazy                  as HashMap
import           Data.Locations
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import qualified Data.Text.IO                       as T
import qualified Data.Yaml                          as Y
import           Options.Applicative
import           System.Directory
import           System.Environment                 (getArgs, withArgs)
import           System.Logger                      (LoggerScribeParams (..), Severity (..))
import           System.TaskPipeline.CLI.Overriding


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
  -> (Maybe FilePath -> IO (Parser (Maybe (a, PipelineCommand r, LoggerScribeParams), IO ())))
  -> ((a, PipelineCommand r, LoggerScribeParams) -> IO r)
  -> IO r
withCliParser progName cliParser f = do
  mbArgs <- tryGetConfigFileOnCLI $ \yamlFile ->
    cliParser yamlFile >>= execCliParser progName "Run a task pipeline"
  case mbArgs of
    Just a  -> f a
    Nothing -> return mempty

-- | Creates a command line parser that will return an action returning the
-- configuration and the chosen subcommand or Nothing if the user simply asked
-- to save some overrides and that the program should stop. It _does not_ mean
-- that an error has occured, just that the program should not continue.
cliYamlParser
  :: (ToJSON cfg)
  => String                   -- ^ The program name
  -> FilePath                 -- ^ Configuration file
  -> cfg                      -- ^ Default configuration
  -> CLIOverriding cfg overrides
  -> [(Parser cmd, String, String)]  -- ^ [(Parser cmd, Command repr, Command help string)]
  -> cmd                      -- ^ Default command
  -> IO (Parser (Maybe (cfg, cmd, LoggerScribeParams), IO ()))
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
  -> Parser (a, IO ())
  -> IO a
execCliParser header_ progDesc_ parser_ = do
  let opts = info (helper <*> parser_)
        ( fullDesc
          <> header header_
          <> progDesc progDesc_ )
  (res, actions) <- execParser opts
  actions
  return res

pureCliParser
  :: (ToJSON cfg)
  => String                   -- ^ The program name
  -> Maybe Y.Value            -- ^ Config read
  -> FilePath                 -- ^ Configuration file
  -> cfg                      -- ^ Default configuration
  -> CLIOverriding cfg overrides
  -> [(Parser cmd, String, String)]  -- ^ [(Parser cmd, Command repr, Command help string)]
  -> cmd                      -- ^ Default command
  -> Parser (Maybe (cfg, cmd, LoggerScribeParams), IO ()) -- ^ (Config and command, actions to run to
                                      -- override the yaml file)
pureCliParser progName mcfg configFile defCfg cfgCLIParsing cmds defCmd =
  subparser
  ( command "write-config-template"
    (info
      (pure (Nothing, writeConfigFile configFile defCfg))
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

writeConfigFile :: ToJSON f => String -> f -> IO ()
writeConfigFile configFile cfg = do
  Y.encodeFile configFile cfg
  putStrLn $ "Wrote " <> configFile

handleOptions
  :: forall cfg cmd overrides.
     (ToJSON cfg)
  => String -- ^ Program name
  -> FilePath -- ^ Config file
  -> Maybe Y.Value -- ^ Config body. This is the JSON content of the config file
  -> cfg           -- ^ The default configuration
  -> CLIOverriding (LoggerScribeParams, cfg) (LoggerScribeParams, overrides)
  -> Maybe (cmd, String) -- ^ Command to run (and a name/description for it)
  -> Bool -- ^ Whether to save the overrides
  -> (LoggerScribeParams, overrides) -- ^ overrides
  -> (Maybe (cfg, cmd, LoggerScribeParams), IO ())  -- ^ (Config and command, actions to run to
                                -- override the yaml file)
handleOptions progName _ Nothing _ _ Nothing _ _ = error $
  "No config found and nothing to save. Please run `" ++ progName ++ " write-config-template' first."
handleOptions progName configFile mbCfg defCfg cliOverriding mbCmd saveOverridesAlong overrides =
  let defaultCfg = toJSON defCfg
      (cfgWarnings, cfg) = case mbCfg of
        Just c -> mergeWithDefault [] defaultCfg c
        Nothing -> ([configFile ++ " is not found. Treated as empty."]
                   ,defaultCfg)
      (overrideWarnings, mbScribeParamsAndCfgOverriden) =
        overrideCfgFromYamlFile cliOverriding cfg overrides
  in case mbScribeParamsAndCfgOverriden of
    Right (lsp, cfgOverriden) ->
      let quietness = loggerSeverityThreshold lsp
          warningActions =
            when (quietness <= WarningS) $ do
            forM_ (cfgWarnings ++ overrideWarnings) $
              putStrLn . ("WARNING: "++)
      in case mbCmd of
          Nothing -> (Nothing, do warningActions
                                  writeConfigFile configFile cfgOverriden)
          Just (cmd, cmdShown) ->
            let actions = do
                  warningActions
                  when saveOverridesAlong $
                    writeConfigFile configFile cfgOverriden
                  when (quietness <= InfoS) $ do
                    T.putStrLn $ "### Running `" <> T.pack progName
                      <> " " <> T.pack cmdShown <> "' with the following config: ###\n"
                      <> T.decodeUtf8 (Y.encode cfgOverriden)
            in (Just (cfgOverriden, cmd, lsp), actions)
    Left err -> dispErr err
  where
    dispErr err = error $
      (if nullOverrides cliOverriding overrides
       then "C"
       else "Overriden c") ++ "onfig from " <> configFile <> " is not valid:\n  " ++ err

mergeWithDefault :: [T.Text] -> Y.Value -> Y.Value -> ([String], Y.Value)
mergeWithDefault path (Object o1) (Object o2) =
  let newKeys = fmap fst . HashMap.toList $ HashMap.difference o2 o1
      warnings = fmap (\key -> T.unpack $
                    "The key " <> T.intercalate "." (key:path) <>
                    "is present in the config but not in the schema." <>
                    "This is probably an error")
                  newKeys
      (subWarnings, merged)
        = sequenceA $ HashMap.unionWithKey
                        (\key (_,v1) (_,v2) -> mergeWithDefault (key:path) v1 v2)
                        (fmap pure o1)
                        (fmap pure o2)
  in
  (warnings ++ subWarnings, Object merged)
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
  => (cfg -> CLIOverriding cfg overrides)
  -> String
  -> FilePath
  -> cfg
  -> IO (Parser (Maybe (cfg, PipelineCommand r, LoggerScribeParams), IO ()))
pipelineCliParser getCliOverriding progName configFile defCfg =
  cliYamlParser progName configFile defCfg (getCliOverriding defCfg)
  [(pure RunPipeline, "run", "Run the pipeline")
  ,(parseShowLocTree, "show-locations", "Show the location tree of the pipeline")]
  RunPipeline
