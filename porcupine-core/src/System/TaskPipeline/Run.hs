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
  , (<--)
  , baseContexts
  , runPipelineTask
  , runPipelineTask_
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
import           System.Environment                 (lookupEnv)
import           System.Exit
import           System.FilePath                    ((</>))
import           System.Posix.Directory             (getWorkingDirectory)
import           System.TaskPipeline.CLI
import           System.TaskPipeline.Logger         (runLogger)
import           System.TaskPipeline.PTask
import           System.TaskPipeline.PTask.Internal
import           System.TaskPipeline.ResourceTree


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
  -> IO o -- , RscAccessTree (ResourceTreeNode m))
                       -- ^ The pipeline task output and the final LocationTree
runPipelineTask = runPipelineTaskWithExceptionHandlers []

runPipelineTaskWithExceptionHandlers
  :: (AcceptableArgsAndContexts args ctxs m)
  => [Handler IO o] -- ^ Exception handlers in case the pipeline raises
                    -- an exception.
                    -- Uncatched exceptions will be printed on stderr and
                    -- cause the executable to exit with status code 1
  -> PipelineConfigMethod o  -- ^ Whether to use the CLI and load the yaml
                             -- config or not
  -> Rec (FieldWithAccessors (ReaderSoup ctxs)) args  -- ^ The location
                                                      -- accessors to use
  -> PTask (ReaderSoup ctxs) i o  -- ^ The whole pipeline task to run
  -> i                 -- ^ The pipeline task input
  -> IO o -- , RscAccessTree (ResourceTreeNode m))
                       -- ^ The pipeline task output and the final LocationTree
runPipelineTaskWithExceptionHandlers exceptionHandlers cliUsage accessors ptask input = do
  let
    tree = ptask ^. ptaskRequirements
    defaultExceptionHandler :: SomeException -> IO a
    defaultExceptionHandler (SomeException e) = do
      putStrLn $ displayException e
      exitWith $ ExitFailure 1
  catches
    (bindResourceTreeAndRun cliUsage accessors tree $
      runPipelineCommandOnPTask ptask input)
    (exceptionHandlers ++ [Handler defaultExceptionHandler])

-- | Like 'runPipelineTask' if the task is self-contained and doesn't have a
-- specific input and you don't need any specific LocationAccessor aside
-- "resource".
runPipelineTask_
  :: PipelineConfigMethod o
  -> PTask (ReaderSoup BasePorcupineContexts) () o
  -> IO o
runPipelineTask_ cliUsage ptask =
  runPipelineTask cliUsage (baseContexts $ cliUsage ^. pipelineConfigMethodProgName) ptask ()

-- pipelineConfigMethodChangeResult
--   :: PipelineConfigMethod o
--   -> PipelineConfigMethod (o, RscAccessTree (ResourceTreeNode m))
-- pipelineConfigMethodChangeResult cliUsage = case cliUsage of
--   NoConfig r     -> NoConfig r
--   FullConfig s r -> FullConfig s r

-- | Runs the required 'PipelineCommand' on an 'PTask'
runPipelineCommandOnPTask
  :: (CanRunPTask m)
  => PTask m i o
  -> i
  -> PipelineCommand o --, RscAccessTree (ResourceTreeNode m))
  -> PhysicalResourceTree m
  -> FunflowOpts m
  -> m o --, RscAccessTree (PhysicalTreeNode m)
runPipelineCommandOnPTask ptask input cmd physTree ffopts = do
  let (virtualTree, runnable) = ptask ^. splittedPTask
  -- virtualTree is the bare tree of VirtualFiles straight from the
  -- pipeline. physTree is virtualTree after configuration, with embedded data
  -- and mappings applied
  case cmd of
    RunPipeline -> do
      dataTree <- traverse resolveDataAccess physTree
      withPTaskState ffopts dataTree $ \initState -> do
        $(logTM) NoticeS $ logStr $ case flowIdentity ffopts of
          Just i -> "Using funflow store at '" ++ storePath ffopts ++ "' with identity "
            ++ show i ++ "." ++
            (case remoteCacheLoc ffopts of
               Just l -> "Using remote cache at " ++ show l
               _      -> "")
          Nothing -> "FUNFLOW_IDENTITY not specified. The cache will not be used."
        execRunnablePTask runnable initState input
    ShowLocTree mode -> do
      liftIO $ putStrLn $ case mode of
        NoMappings   -> prettyLocTree virtualTree
        FullMappings -> prettyLocTree physTree
      return mempty

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
  => ResourceTreeAndMappings
  -> AvailableAccessors m
  -> m (PhysicalResourceTree m, FunflowOpts m)
getPhysTreeAndFFOpts rtam accessors =
  flip runReaderT accessors $
    (,) <$> getPhysicalResourceTreeFromMappings rtam
        <*> getFunflowOpts

-- | Runs the cli if using FullConfig, binds every location in the resource tree
-- to its final value/path, and passes the continuation the bound resource tree.
bindResourceTreeAndRun
  :: (AcceptableArgsAndContexts args ctxs m)
  => PipelineConfigMethod r -- ^ How to get CLI args from ModelOpts
  -> Rec (FieldWithAccessors (ReaderSoup ctxs)) args
  -> VirtualResourceTree    -- ^ The tree to look for DocRecOfoptions in
  -> (PipelineCommand r
      -> PhysicalResourceTree (ReaderSoup ctxs)
      -> FunflowOpts (ReaderSoup ctxs)
      -> ReaderSoup ctxs r)
             -- ^ What to do with the tree
  -> IO r
bindResourceTreeAndRun (NoConfig _ root) accessorsRec tree f =
  consumeSoup argsRec $ do
    (physTree, ffPaths) <- getPhysTreeAndFFOpts rtam accessors
    f RunPipeline physTree ffPaths
  where
    rtam = ResourceTreeAndMappings tree (Left root) mempty
    (accessors, argsRec) = splitAccessorsFromArgRec accessorsRec
bindResourceTreeAndRun (FullConfig progName defConfigFileURL defRoot) accessorsRec tree f =
  withConfigFileSourceFromCLI $ \mbConfigFileSource -> do
    let configFileSource = fromMaybe (ConfigFileURL (LocalFile defConfigFileURL)) mbConfigFileSource
    mbConfig <- tryReadConfigFileSource configFileSource $ \remoteURL ->
                  consumeSoup argsRec $ do
                    -- If config file is remote, we use the accessors and run
                    -- the readerSoup with the defaut katip params
                    SomeGLoc loc <- flip runReaderT accessors $
                                    resolvePathToSomeLoc $ show remoteURL
                      -- TODO: Implement locExists in each accessor and use it
                      -- here. For now we fail if given a remote config that
                      -- doesn't exist.
                    readBSS loc decodeYAMLStream_
    parser <- pipelineCliParser rscTreeConfigurationReader progName $
              BaseInputConfig (case configFileSource of
                                 ConfigFileURL (LocalFile filep) -> Just filep
                                 _                               -> Nothing)
                              mbConfig
                              (ResourceTreeAndMappings tree (Left defRoot) mempty)
    withCliParser progName "Run a task pipeline" parser run
  where
    (accessors, argsRec) = splitAccessorsFromArgRec accessorsRec
    run rtam cmd lsp performConfigWrites =
      let -- We change the katip runner, from the options we got from CLI:
          argsRec' = argsRec & set (rlensf #katip)
            (ContextRunner (runLogger progName lsp))
      in
      consumeSoup argsRec' $ do
        unPreRun performConfigWrites
        (physTree, ffPaths) <- getPhysTreeAndFFOpts rtam accessors
        f cmd physTree ffPaths
