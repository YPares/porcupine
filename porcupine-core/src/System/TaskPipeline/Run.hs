{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module System.TaskPipeline.Run
  ( PipelineConfigMethod(..)
  , PipelineCommand(..)
  , PipelineTask
  , runPipelineTask
  , runPipelineTask_
  , runPipelineCommandOnPTask
  ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Locations
import           Data.Maybe
import           Katip
import           System.Exit
import           System.TaskPipeline.CLI
import           System.TaskPipeline.Logger (LoggerScribeParams,
                                             defaultLoggerScribeParams,
                                             runLogger)
import           System.TaskPipeline.Tasks


-- | A task defining a whole pipeline, and that may run in any LocationMonad. It
-- is an Arrow, which means you obtain it by composing subtasks either
-- sequentially with '(>>>)' (or '(.)'), or in parallel with '(***)'.
type PipelineTask i o =
     forall m. (KatipContext m, LocationMonad m, MonadIO m)
  => PTask m i o
  -- MonadIO constraint is meant to be temporary (that's needed for the
  -- pipelines who do time tracking for instance, but that shouldn't be done
  -- like that).

-- | Runs an 'PTask' according to some 'PipelineConfigMethod' and with an input
-- @i@. In principle, it should be directly called by your @main@ function. It
-- exits with @ExitFailure 1@ when the 'PipelineTask' raises an uncatched
-- exception.
runPipelineTask
  :: String            -- ^ The program name (for CLI --help)
  -> PipelineConfigMethod o  -- ^ Whether to use the CLI and load the yaml
                             -- config or not
  -> PipelineTask i o  -- ^ The whole pipeline task to run
  -> i                 -- ^ The pipeline task input
  -> IO o -- , RscAccessTree (ResourceTreeNode m))
                       -- ^ The pipeline task output and the final LocationTree
runPipelineTask progName cliUsage ptask input =
  -- let cliUsage' = pipelineConfigMethodChangeResult cliUsage
  catch
    (bindResourceTreeAndRun progName cliUsage (pTaskResourceTree ptask)
      (runPipelineCommandOnPTask ptask input)
      (selectRun' progName))
    (\(SomeException e) -> do
        putStrLn $ displayException e
        exitWith $ ExitFailure 1)

-- | Like 'runPipelineTask' if the task is self-contained and doesn't have a
-- specific input, and if you don't care about the final LocationTree
runPipelineTask_
  :: String
  -> PipelineConfigMethod o
  -> PipelineTask () o
  -> IO o
runPipelineTask_ name cliUsage ptask =
  -- fst <$>
  runPipelineTask name cliUsage ptask ()

-- pipelineConfigMethodChangeResult
--   :: PipelineConfigMethod o
--   -> PipelineConfigMethod (o, RscAccessTree (ResourceTreeNode m))
-- pipelineConfigMethodChangeResult cliUsage = case cliUsage of
--   NoConfig r     -> NoConfig r
--   FullConfig s r -> FullConfig s r

-- | Runs the required 'PipelineCommand' on an 'PTask'
runPipelineCommandOnPTask
  :: (LocationMonad m, KatipContext m)
  => PTask m i o
  -> i
  -> PipelineCommand o --, RscAccessTree (ResourceTreeNode m))
  -> PhysicalResourceTree m
  -> m o --, RscAccessTree (PhysicalTreeNode m)
runPipelineCommandOnPTask (PTask origTree taskFn) input cmd boundTree =
  -- origTree is the bare tree straight from the pipeline. boundTree is origTree
  -- after configuration, with embedded data and mappings updated
  case cmd of
    RunPipeline -> do
      dataTree <- traverse resolveDataAccess boundTree
      fst <$> taskFn (input, fmap (RscAccess 0) dataTree)
    ShowLocTree mode -> do
      liftIO $ putStrLn $ case mode of
        NoMappings   -> prettyLocTree origTree
        FullMappings -> prettyLocTree boundTree
      return mempty

-- | Runs the cli if using FullConfig, binds every location in the resource tree
-- to its final value/path, and passes the bound resource tree to the
-- continuation.
bindResourceTreeAndRun
  :: (KatipContext m, MonadIO m)
  => String   -- ^ Program name (often model name)
  -> PipelineConfigMethod r -- ^ How to get CLI args from ModelOpts
  -> VirtualResourceTree m  -- ^ The tree to look for DocRecOfoptions in
  -> (PipelineCommand r -> PhysicalResourceTree m -> m r)
                            -- ^ What to do with the bound resource tree
  -> (forall a. Either Loc LocationMappings -> LoggerScribeParams -> m a -> IO a)
  -> IO r
bindResourceTreeAndRun progName (NoConfig root) tree f runMonad =
  -- selectRun root True $
  --   runLogger progName defaultLoggerScribeParams $
  runMonad (Left root) defaultLoggerScribeParams $
      f RunPipeline $
        getPhysicalResourceTreeFromMappings $ ResourceTreeAndMappings tree (Left root) mempty
bindResourceTreeAndRun progName (FullConfig defConfigFile defRoot) tree f runMonad =
  withCliParser progName "Run a task pipeline" getParser run
  where
    getParser mbConfigFile =
      pipelineCliParser rscTreeConfigurationReader progName
        (fromMaybe defConfigFile mbConfigFile)
        (ResourceTreeAndMappings tree (Left defRoot) mempty)
    run rtam@(ResourceTreeAndMappings{rtamMappings=mappings'}) cmd lsp performConfigWrites =
      runMonad mappings' lsp $ do
      -- selectRun refLoc True $
      --   runLogger progName lsp $ do
          unPreRun performConfigWrites
          f cmd $ getPhysicalResourceTreeFromMappings rtam
      -- where
      --   refLoc = case mappings' of
      --     Left rootLoc -> fmap (const ()) rootLoc
      --     Right m      -> refLocFromMappings m

selectRun' :: String
           -> Either Loc LocationMappings
           -> LoggerScribeParams
           -> (forall m. (LocationMonad m, MonadIO m) => m a)
           -> IO a
selectRun' progName mappings lsp f =
  selectRun refLoc True $ runLogger progName lsp f
  where
    refLoc = case mappings of
      Left rootLoc -> fmap (const ()) rootLoc
      Right m      -> refLocFromMappings m
  
refLocFromMappings :: LocationMappings -> Loc_ ()
refLocFromMappings m = foldr f (LocalFile $ LocFilePath () "")
                               (map (fmap (const ())) $ allLocsInMappings m)
  where
    f a@(S3Obj{}) _ = a
    f _ b@(S3Obj{}) = b
    f a _           = a
