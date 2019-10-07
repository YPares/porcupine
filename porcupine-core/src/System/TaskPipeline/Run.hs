{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_GHC "-fno-warn-missing-signatures" #-}

module System.TaskPipeline.Run
  ( PipelineConfigMethod(..)
  , PipelineCommand(..)
  , CanRunPTask
  , Rec(..)
  , ContextRunner(..)
  , SimplePTaskM, BasePorcupineContexts
  , ReaderSoup
  , FieldWithAccessors
  , AcceptableArgsAndContexts
  , (<--)
  , (:::)
  , baseContexts, baseContextsWithScribeParams
  , maxVerbosityLoggerScribeParams
  , warningsAndErrorsLoggerScribeParams
  , runPipelineTask
  , runLocalPipelineTask
  , simpleRunPTask
  , runPipelineTaskWithExceptionHandlers
  , runPipelineCommandOnPTask
  ) where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.ReaderSoup
import           Control.Monad.ReaderSoup.Katip     ()
import           Data.Locations                     hiding ((</>))
import           Data.Locations.Accessors
import           Data.Maybe
import           Data.Vinyl.Derived                 (HasField, rlensf)
import           Katip
import           Prelude                            hiding ((.))
import           System.Environment                 (lookupEnv, withArgs)
import           System.FilePath                    ((</>))
import           System.Posix.Directory             (getWorkingDirectory)
import           System.TaskPipeline.CLI
import           System.TaskPipeline.Logger
import           System.TaskPipeline.PorcupineTree
import           System.TaskPipeline.PTask
import           System.TaskPipeline.PTask.Internal


-- | Tells whether a record of args can be used to run a PTask
type AcceptableArgsAndContexts args ctxs m =
  (ArgsForSoupConsumption args, ctxs ~ ContextsFromArgs args
  ,IsInSoup ctxs "katip", IsInSoup ctxs "resource"
  ,RunsKatipOver args m)

-- | We need to have some information about how katip will be run, because we
-- will want to override that from the command-line
type RunsKatipOver args m =
  (HasField Rec "katip" args args (ContextRunner KatipContextT m) (ContextRunner KatipContextT m)
  ,MonadMask m, MonadIO m)

-- | Runs an 'PTask' according to some 'PipelineConfigMethod' and with an input
-- @i@. In principle, it should be directly called by your @main@ function. It
-- exits with @ExitFailure 1@ when the 'PipelineTask' raises an uncatched
-- exception.
runPipelineTask
  :: (AcceptableArgsAndContexts args ctxs m)
  => PipelineConfigMethod o  -- ^ Whether to use the CLI and load the yaml
                             -- config or not
  -> Rec (FieldWithAccessors (ReaderSoup ctxs)) args  -- ^ The location
                                                      -- accessors to use
  -> PTask (ReaderSoup ctxs) i o  -- ^ The whole pipeline task to run
  -> i                 -- ^ The pipeline task input
  -> IO o              -- ^ The pipeline task output
runPipelineTask = runPipelineTaskWithExceptionHandlers []

runPipelineTaskWithExceptionHandlers
  :: (AcceptableArgsAndContexts args ctxs m)
  => [Handler IO o] -- ^ Exception handlers in case the pipeline raises
                    -- an exception.
  -> PipelineConfigMethod o  -- ^ Whether to use the CLI and load the yaml
                             -- config or not
  -> Rec (FieldWithAccessors (ReaderSoup ctxs)) args  -- ^ The location
                                                      -- accessors to use
  -> PTask (ReaderSoup ctxs) i o  -- ^ The whole pipeline task to run
  -> i                 -- ^ The pipeline task input
  -> IO o              -- ^ The pipeline task output
runPipelineTaskWithExceptionHandlers exceptionHandlers configMethod accessors ptask input = do
  let tree = ptask ^. taskRequirements
  catches
    (bindVirtualTreeAndRun configMethod accessors tree $
      runPipelineCommandOnPTask ptask input)
    exceptionHandlers

-- | A monad that implements MonadIO, MonadUnliftIO, KatipContext and
-- MonadResource. For simplest uses of porcupine.
type SimplePTaskM = ReaderSoup BasePorcupineContexts

-- | Like 'runPipelineTask' if you don't need to access any other resources than
-- local files. Uses the 'maxVerbosityLoggerScribeParams' by default.
runLocalPipelineTask
  :: PipelineConfigMethod o
  -> PTask SimplePTaskM i o
  -> i
  -> IO o
runLocalPipelineTask configMethod =
  runPipelineTask configMethod (baseContexts $ configMethod ^. pipelineConfigMethodProgName)

-- | Runs a PTask without reading any configuration nor parsing the CLI, with
-- only local files being accessible, and using PWD as the root location for all
-- files read and written.
simpleRunPTask
  :: PTask SimplePTaskM i o
  -> i
  -> IO o
simpleRunPTask = runLocalPipelineTask (NoConfig "simpleRunPTask" ".")

-- | Runs the required 'PipelineCommand' on an 'PTask'
runPipelineCommandOnPTask
  :: (CanRunPTask m)
  => PTask m i o
  -> i
  -> PipelineCommand
  -> Maybe o
  -> PhysicalTree m
  -> FunflowOpts m
  -> m o
runPipelineCommandOnPTask ptask input cmd defRetVal physTree ffopts = do
  case cmd of
    RunPipeline -> do
      dataTree <- traverse resolveDataAccess physTree
      withTaskState ffopts dataTree $ \initState -> do
        $(logTM) NoticeS $ logStr $ case flowIdentity ffopts of
          Just i -> "Using funflow store at '" ++ storePath ffopts ++ "' with identity "
            ++ show i ++ "." ++
            (case remoteCacheLoc ffopts of
               Just l -> "Using remote cache at " ++ show l
               _      -> "")
          Nothing -> identityVar ++ " not specified. The cache will not be used."
        execRunnableTask (ptask ^. taskRunnablePart) initState input
    ShowTree root showOpts -> do
      liftIO $ putStrLn $ prettyLocTree root $
        case physTree ^. inLocTree root of
          Just t -> fmap (PhysicalFileNodeWithShowOpts showOpts) t
          _ -> error $ "Path `" ++ showLTP root ++ "' doesn't exist in the porcupine tree"
      case defRetVal of
        Just r -> return r
        Nothing -> error "NOT EXPECTED: runPipelineCommandOnPTask(ShowTree) was not given a default\
                         \value to return. Please submit this as a bug."

storeVar,remoteCacheVar,identityVar,coordVar :: String
storeVar       = "FUNFLOW_STORE"
remoteCacheVar = "FUNFLOW_REMOTE_CACHE"
coordVar       = "FUNFLOW_COORDINATOR"
identityVar    = "FUNFLOW_IDENTITY"

-- | Reads the relevant environment variables to construct the set of parameters
-- necessary to initialize funflow
getFunflowOpts :: (MonadIO m, LogThrow m) => LocResolutionM m (FunflowOpts m)
getFunflowOpts = do
  pwd <- liftIO getWorkingDirectory
  givenStore <- lookupEnv' storeVar
  opts <- FunflowOpts
        (fromMaybe (pwd </> "_funflow/store") givenStore)
    <$> (fromMaybe (pwd </> "_funflow/coordinator.db") <$> lookupEnv' coordVar)
    <*> (lookupEnv' identityVar >>= parseIdentity)
    <*> (lookupEnv' remoteCacheVar >>= traverse resolveYamlDocToSomeLoc)
  case (flowIdentity opts, givenStore, remoteCacheLoc opts) of
    (Nothing, Just _, _) -> warnAboutIdentity storeVar
    (Nothing, _, Just _) -> warnAboutIdentity remoteCacheVar
    _                    -> return ()
  return opts
  where
    lookupEnv' = liftIO . lookupEnv
    parseIdentity Nothing = return Nothing
    parseIdentity (Just "") = return Nothing
    parseIdentity (Just s) = case reads s of
      [(i,_)] -> return $ Just i
      _       -> fail $ identityVar ++ " isn't a valid integer"
    warnAboutIdentity var = $(logTM) WarningS $ logStr $
      var ++ " has been given but no " ++ identityVar ++
      " has been provided. Caching will NOT be performed."

-- | Resolve all the JSON values in the mappings and paths from environment (for
-- funflow) to locations tied to their respective LocationAccessors
getPhysTreeAndFFOpts
  :: (MonadIO m, LogThrow m)
  => VirtualTreeAndMappings
  -> AvailableAccessors m
  -> m (PhysicalTree m, FunflowOpts m)
getPhysTreeAndFFOpts vtam accessors =
  flip runReaderT accessors $
    (,) <$> getPhysicalTreeFromMappings vtam
        <*> getFunflowOpts

-- | Runs the cli if using FullConfig, binds every location in the virtual tree
-- to its final value/path, and passes to the continuation the physical tree.
bindVirtualTreeAndRun
  :: (AcceptableArgsAndContexts args ctxs m)
  => PipelineConfigMethod r -- ^ How to read the configuration
  -> Rec (FieldWithAccessors (ReaderSoup ctxs)) args
  -> VirtualTree    -- ^ The tree to look for DocRecOfoptions in
  -> (PipelineCommand
      -> Maybe r
      -> PhysicalTree (ReaderSoup ctxs)
      -> FunflowOpts (ReaderSoup ctxs)
      -> ReaderSoup ctxs r)
             -- ^ What to do with the tree
  -> IO r
bindVirtualTreeAndRun (NoConfig _ root) accessorsRec tree f =
  consumeSoup argsRec $ do
    (physTree, ffPaths) <- getPhysTreeAndFFOpts defaultConfig accessors
    f RunPipeline Nothing physTree ffPaths
  where
    defaultConfig = VirtualTreeAndMappings tree (Left root) mempty
    (accessors, argsRec) = splitAccessorsFromArgRec accessorsRec
bindVirtualTreeAndRun (ConfigFileOnly progName configFileURL defRoot) accessorsRec tree f = do
  -- We deactivate every argument that might have been passed so the only choice
  -- is to run the pipeline. Given the parsing of the config file and the
  -- command-line are quite related, it is difficult to just remove the CLI
  -- parsing until that part of the code is refactored to better separate CLI
  -- parsing and deserialization of the VirtualTreeAndMappings from the config
  -- file
  res <- withArgs ["-qq", "--context-verb", "2", "--log-format", "compact"] $
         -- No CLI arg is passable, so until we improve CLI parsin as stated
         -- just above, in that case we limit ourselves to warnings and errors
    bindVirtualTreeAndRun (FullConfig progName configFileURL defRoot Nothing) accessorsRec tree $
      \_ _ t o -> Just <$> f RunPipeline Nothing t o
  case res of
    Just r -> return r
    Nothing -> error "NOT EXPECTED: bindVirtualTreeAndRun(ConfigFileOnly) didn't receive a result\
                     \from the pipeline. Please submit this as a bug."
bindVirtualTreeAndRun (FullConfig progName defConfigFileURL defRoot defRetVal) accessorsRec tree f =
  withConfigFileSourceFromCLI $ \mbConfigFileSource -> do
    let configFileSource = fromMaybe (ConfigFileURL (LocalFile defConfigFileURL)) mbConfigFileSource
    mbConfigFromFile <-
      tryReadConfigFileSource configFileSource $ \remoteURL ->
                  consumeSoup argsRec $ do
                    -- If config file is remote, we use the accessors and run
                    -- the readerSoup with the defaut katip params
                    SomeGLoc loc <- flip runReaderT accessors $
                                    resolvePathToSomeLoc $ show remoteURL
                      -- TODO: Implement locExists in each accessor and use it
                      -- here. For now we fail if given a remote config that
                      -- doesn't exist.
                    readBSS loc decodeYAMLStream
    parser <- pipelineCliParser virtualTreeConfigurationReader progName $
              BaseInputConfig (case configFileSource of
                                 ConfigFileURL (LocalFile filep) -> Just filep
                                 _                               -> Nothing)
                              mbConfigFromFile
                              defaultConfig
    withCliParser progName "Run a task pipeline" parser defRetVal run
  where
    defaultConfig = VirtualTreeAndMappings tree (Left defRoot) mempty
    (accessors, argsRec) = splitAccessorsFromArgRec accessorsRec
    run finalConfig cmd lsp performConfigWrites =
      let -- We change the katip runner, from the options we got from CLI:
          argsRec' = argsRec & set (rlensf #katip)
            (ContextRunner (runLogger progName lsp))
      in
      consumeSoup argsRec' $ do
        unPreRun performConfigWrites
        (physTree, ffPaths) <- getPhysTreeAndFFOpts finalConfig accessors
        f cmd (Just defRetVal) physTree ffPaths
