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
runPipelineTask cliUsage accessors ptask input = do
  let -- cliUsage' = pipelineConfigMethodChangeResult cliUsage
      tree = ptask ^. ptaskRequirements
  catch
    (bindResourceTreeAndRun cliUsage accessors tree $
      runPipelineCommandOnPTask ptask input)
    (\(SomeException e) -> do
        putStrLn $ displayException e
        exitWith $ ExitFailure 1)

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
  -> FunflowPaths m
  -> m o --, RscAccessTree (PhysicalTreeNode m)
runPipelineCommandOnPTask ptask input cmd physTree ffPaths = do
  let (virtualTree, runnable) = ptask ^. splittedPTask
  -- virtualTree is the bare tree of VirtualFiles straight from the
  -- pipeline. physTree is virtualTree after configuration, with embedded data
  -- and mappings applied
  case cmd of
    RunPipeline -> do
      dataTree <- traverse resolveDataAccess physTree
      withPTaskState ffPaths dataTree $ \initState -> do
        $(logTM) DebugS $ logStr $ "Using funflow store in '" ++ storePath ffPaths
              ++ "' and coordinator '" ++ coordPath ffPaths ++ "'."
        execRunnablePTask runnable initState input
    ShowLocTree mode -> do
      liftIO $ putStrLn $ case mode of
        NoMappings   -> prettyLocTree virtualTree
        FullMappings -> prettyLocTree physTree
      return mempty

getFunflowPaths :: (MonadIO m, LogThrow m) => LocResolutionM m (FunflowPaths m)
getFunflowPaths = do
  pwd <- liftIO getWorkingDirectory
  FunflowPaths
    <$> (fromMaybe (pwd </> "_funflow/store") <$> lookupEnv' "FUNFLOW_STORE")
    <*> (fromMaybe (pwd </> "_funflow/coordinator.db") <$> lookupEnv' "FUNFLOW_COORDINATOR")
    <*> (lookupEnv' "FUNFLOW_REMOTE_CACHE" >>= traverse resolvePathToSomeLoc)
  where
    lookupEnv' = liftIO . lookupEnv

-- | Resolve all the JSON values in the mappings and paths from environment (for
-- funflow) to locations tied to their respective LocationAccessors
getPhysTreeAndFFPaths
  :: (MonadIO m, LogThrow m)
  => ResourceTreeAndMappings
  -> AvailableAccessors m
  -> m (PhysicalResourceTree m, FunflowPaths m)
getPhysTreeAndFFPaths rtam accessors =
  flip runReaderT accessors $
    (,) <$> getPhysicalResourceTreeFromMappings rtam
        <*> getFunflowPaths

-- | Runs the cli if using FullConfig, binds every location in the resource tree
-- to its final value/path, and passes the continuation the bound resource tree.
bindResourceTreeAndRun
  :: (AcceptableArgsAndContexts args ctxs m)
  => PipelineConfigMethod r -- ^ How to get CLI args from ModelOpts
  -> Rec (FieldWithAccessors (ReaderSoup ctxs)) args
  -> VirtualResourceTree    -- ^ The tree to look for DocRecOfoptions in
  -> (PipelineCommand r
      -> PhysicalResourceTree (ReaderSoup ctxs)
      -> FunflowPaths (ReaderSoup ctxs)
      -> ReaderSoup ctxs r)
             -- ^ What to do with the tree
  -> IO r
bindResourceTreeAndRun (NoConfig _ root) accessorsRec tree f =
  consumeSoup argsRec $ do
    (physTree, ffPaths) <- getPhysTreeAndFFPaths rtam accessors
    f RunPipeline physTree ffPaths
  where
    rtam = ResourceTreeAndMappings tree (Left root) mempty
    (accessors, argsRec) = splitAccessorsFromArgRec accessorsRec
bindResourceTreeAndRun (FullConfig progName defConfigFileURL defRoot) accessorsRec tree f =
  tryGetConfigFileOnCLI $ \mbConfigFileURL -> do
    let configFileURL = fromMaybe defConfigFileURL mbConfigFileURL
    mbConfig <- tryReadConfigFile configFileURL
    parser <- pipelineCliParser rscTreeConfigurationReader progName $
              BaseInputConfig (Just configFileURL) mbConfig
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
        (physTree, ffPaths) <- getPhysTreeAndFFPaths rtam accessors
        f cmd physTree ffPaths
