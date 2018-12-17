{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC "-fno-warn-missing-signatures" #-}

module System.TaskPipeline.Run
  ( PipelineConfigMethod(..)
  , PipelineCommand(..)
  , CanRunPTask
  , runPipelineTask
  , runPipelineTask_
  , runPipelineCommandOnPTask
  ) where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.ReaderSoup
import           Control.Monad.ReaderSoup.Katip     ()
import           Data.Locations                     hiding ((</>))
import           Data.Locations.Accessors
import           Data.Maybe
import           Data.Vinyl.Derived                 (rlensf, FieldType, HasField)
import           Data.Vinyl.Functor
import           Katip
import           System.Environment                 (getEnv, lookupEnv)
import           System.Exit
import           System.FilePath                    ((</>))
import           System.TaskPipeline.CLI
import           System.TaskPipeline.Logger         (defaultLoggerScribeParams,
                                                     runLogger)
import           System.TaskPipeline.PTask
import           System.TaskPipeline.PTask.Internal
import           System.TaskPipeline.ResourceTree
import           Prelude                            hiding (id, (.))


type AcceptableArgsAndContexts args ctxs m =
  (ArgsForSoupConsumption args, ctxs ~ ContextsFromArgs args
  ,IsInSoup ctxs "katip", IsInSoup ctxs "resource"
  ,RunsKatipOver args m)

-- | We need to have some information about how katip will be run, because we
-- may need to override that from the command-line
type RunsKatipOver args m =
  (HasField Rec "katip" args args (ContextRunner KatipContextT m) (ContextRunner KatipContextT m)
  ,MonadMask m, MonadIO m)

-- | Runs an 'PTask' according to some 'PipelineConfigMethod' and with an input
-- @i@. In principle, it should be directly called by your @main@ function. It
-- exits with @ExitFailure 1@ when the 'PipelineTask' raises an uncatched
-- exception.
runPipelineTask
  :: (AcceptableArgsAndContexts args ctxs m)
  => String            -- ^ The program name (for CLI --help)
  -> PipelineConfigMethod o  -- ^ Whether to use the CLI and load the yaml
                             -- config or not
  -> Rec (FieldWithAccessors (ReaderSoup ctxs)) args  -- ^ The location
                                                      -- accessors to use
  -> PTask (PorcupineM ctxs) i o  -- ^ The whole pipeline task to run
  -> i                 -- ^ The pipeline task input
  -> IO o -- , RscAccessTree (ResourceTreeNode m))
                       -- ^ The pipeline task output and the final LocationTree
runPipelineTask progName cliUsage accessors ptask input = do
  let -- cliUsage' = pipelineConfigMethodChangeResult cliUsage
      tree = ptask ^. splittedPTask . _1
  catch
    (bindResourceTreeAndRun progName cliUsage accessors tree $
      runPipelineCommandOnPTask ptask input)
    (\(SomeException e) -> do
        putStrLn $ displayException e
        exitWith $ ExitFailure 1)

-- | Like 'runPipelineTask' if the task is self-contained and doesn't have a
-- specific input and you don't need any specific LocationAccessor aside
-- "resource".
runPipelineTask_
  :: String
  -> PipelineConfigMethod o
  -> PTask SimplePorcupineM () o
  -> IO o
runPipelineTask_ name cliUsage ptask =
  -- fst <$>
  runPipelineTask name cliUsage baseRec ptask ()
  where
    baseRec = #katip    <-- ContextRunner (runLogger name defaultLoggerScribeParams)
           :& #resource <-- useResource
           :& RNil

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
  -> PhysicalResourceTree
  -> m o --, RscAccessTree (PhysicalTreeNode m)
runPipelineCommandOnPTask ptask input cmd boundTree = do
  let (origTree, runnable) = ptask ^. splittedPTask
  -- origTree is the bare tree straight from the pipeline. boundTree is origTree
  -- after configuration, with embedded data and mappings updated
  case cmd of
    RunPipeline -> do
      dataTree <- traverse resolveDataAccess boundTree
      ffPaths <- liftIO $ do
        pwd <- getEnv "PWD"
        FunflowPaths
          <$> (fromMaybe (pwd </> "_funflow/store") <$> lookupEnv "FUNFLOW_STORE")
          <*> (fromMaybe (pwd </> "_funflow/coordinator.db") <$> lookupEnv "FUNFLOW_COORDINATOR")
          <*> getRemoteCacheLocation
      withPTaskState ffPaths dataTree $ \initState -> do
        $(logTM) DebugS $ logStr $ "Using funflow store in '" ++ storePath ffPaths
              ++ "' and coordinator '" ++ coordPath ffPaths ++ "'."
        execRunnablePTask runnable initState input
    ShowLocTree mode -> do
      liftIO $ putStrLn $ case mode of
        NoMappings   -> prettyLocTree origTree
        FullMappings -> prettyLocTree boundTree
      return mempty

getRemoteCacheLocation :: IO (Maybe Loc)
getRemoteCacheLocation = do
  fromEnv <- fmap parseURLLikeLoc <$> lookupEnv "FUNFLOW_REMOTE_CACHE"
  case fromEnv of
    Just (Right x)  -> pure (Just x)
    Just (Left err) -> error err
    Nothing         -> pure Nothing

-- | Runs the cli if using FullConfig, binds every location in the resource tree
-- to its final value/path, and passes the continuation the bound resource tree.
bindResourceTreeAndRun
  :: (AcceptableArgsAndContexts args ctxs m)
  => String   -- ^ Program name (often model name)
  -> PipelineConfigMethod r -- ^ How to get CLI args from ModelOpts
  -> Rec (FieldWithAccessors (ReaderSoup ctxs)) args
  -> VirtualResourceTree    -- ^ The tree to look for DocRecOfoptions in
  -> (PipelineCommand r -> PhysicalResourceTree -> PorcupineM ctxs r)
             -- ^ What to do with the tree
  -> IO r
bindResourceTreeAndRun _progName (NoConfig root) accessors tree f =
  runPorcupineM accessors $
    f RunPipeline $
      getPhysicalResourceTreeFromMappings $ ResourceTreeAndMappings tree (Left root) mempty
bindResourceTreeAndRun progName (FullConfig defConfigFile defRoot) accessors tree f =
  withCliParser progName "Run a task pipeline" getParser run
  where
    getParser mbConfigFile =
      pipelineCliParser rscTreeConfigurationReader progName
        (fromMaybe defConfigFile mbConfigFile)
        (ResourceTreeAndMappings tree (Left defRoot) mempty)
    run rtam cmd lsp performConfigWrites =
      let (parserCtx, argsRec) = splitAccessorsFromRec accessors
          -- We change the katip runner, from the options we got from CLI:
          argsRec' = argsRec & set (rlensf #katip)
            (ContextRunner (runLogger progName lsp))
      in
      consumeSoup argsRec' $ flip runReaderT parserCtx $ do
        unPreRun performConfigWrites
        f cmd $ getPhysicalResourceTreeFromMappings rtam
