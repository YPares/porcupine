{-# LANGUAGE RankNTypes #-}

module System.TaskPipeline.Run
  ( PipelineConfigMethod(..)
  , PipelineCommand(..)
  , PipelineTask
  , runPipelineTask
  , runPipelineTask_
  , runPipelineCommandOnATask
  ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Locations
import           Data.Maybe
import           Katip                      (KatipContext, KatipContextT)
import           System.Exit
import           System.TaskPipeline.CLI
import           System.TaskPipeline.Logger (defaultLoggerScribeParams,
                                             runLogger)
import           System.TaskPipeline.Tasks


-- | A task defining a whole pipeline, and that may run in any LocationMonad. It
-- is an Arrow, which means you obtain it by composing subtasks either
-- sequentially with '(>>>)' (or '(.)'), or in parallel with '(***)'.
type PipelineTask i o =
     forall m. (KatipContext m, LocationMonad m, MonadIO m)
  => ATask m PipelineResource i o
  -- MonadIO constraint is meant to be temporary (that's needed for the
  -- pipelines who do time tracking for instance, but that shouldn't be done
  -- like that).

-- | Runs an 'ATask' according to some 'PipelineConfigMethod' and with an input
-- @i@. In principle, it should be directly called by your @main@ function. It
-- exits with @ExitFailure 1@ when the 'PipelineTask' raises an uncatched
-- exception.
runPipelineTask
  :: String            -- ^ The program name (for CLI --help)
  -> PipelineConfigMethod o  -- ^ Whether to use the CLI and load the yaml
                             -- config or not
  -> PipelineTask i o  -- ^ The whole pipeline task to run
  -> i                 -- ^ The pipeline task input
  -> IO (o, RscAccessTree BoundPipelineResource)
                       -- ^ The pipeline task output and the final LocationTree
runPipelineTask progName cliUsage atask input = do
  let cliUsage' = pipelineConfigMethodChangeResult cliUsage
      tree = getTaskTree atask
        -- We temporarily instanciate atask so we can get the
        -- tree (which doesn't depend anyway on the
        -- monad). That's just to make GHC happy.
  catch
    (bindResourceTreeAndRun progName cliUsage' tree $
      runPipelineCommandOnATask atask input)
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
runPipelineTask_ name cliUsage atask =
  fst <$> runPipelineTask name cliUsage atask ()

pipelineConfigMethodChangeResult
  :: PipelineConfigMethod o
  -> PipelineConfigMethod (o, RscAccessTree BoundPipelineResource)
pipelineConfigMethodChangeResult cliUsage = case cliUsage of
  NoConfig r     -> NoConfig r
  FullConfig s r -> FullConfig s r

getTaskTree
  :: ATask (KatipContextT LocalM) PipelineResource i o
  -> UnboundResourceTree
getTaskTree (ATask t _) = t

-- | Runs the required 'PipelineCommand' on an 'ATask'
runPipelineCommandOnATask
  :: (LocationMonad m)
  => ATask m PipelineResource i o
  -> i
  -> PipelineCommand (o, RscAccessTree BoundPipelineResource)
  -> BoundResourceTree
  -> m (o, RscAccessTree BoundPipelineResource)
runPipelineCommandOnATask (ATask origTree taskFn) input cmd boundTree =
  -- origTree is the bare tree straight from the pipeline
  -- boundTree is origTree after configuration, with options and mappings updated
  case cmd of
    RunPipeline -> taskFn (input, fmap AccessPending boundTree)
    ShowLocTree mode -> do
      logMsg $ case mode of
        NoMappings   -> prettyLocTree origTree
        FullMappings -> prettyLocTree boundTree
      return mempty

-- | Runs the cli if using FullConfig, binds every location in the resource tree
-- to its final value/path, and passes the continuation the bound resource tree.
bindResourceTreeAndRun
  :: String   -- ^ Program name (often model name)
  -> PipelineConfigMethod r -- ^ How to get CLI args from ModelOpts
  -> UnboundResourceTree -- ^ The tree to look for DocRecOfoptions in
  -> (forall m. (KatipContext m, LocationMonad m, MonadIO m)
      => PipelineCommand r -> BoundResourceTree -> m r)
             -- ^ What to do with the model
  -> IO r
bindResourceTreeAndRun _ (NoConfig root) tree f =
  selectRun root True $
    runLogger defaultLoggerScribeParams $
      f RunPipeline $
        applyMappingsToResourceTree' tree (Left root)
bindResourceTreeAndRun progName (FullConfig defConfigFile defRoot) tree f =
  withCliParser progName getParser run
  where
    getParser mbConfigFile =
      pipelineCliParser rscTreeBasedCLIOverriding progName
        (fromMaybe defConfigFile mbConfigFile)
        (ResourceTreeAndMappings tree $ Left defRoot)
    run (tam@(ResourceTreeAndMappings _ mappings'), cmd, lsp) =
      selectRun refLoc True $
        runLogger lsp $
          f cmd $ applyMappingsToResourceTree tam
      where
        refLoc = case mappings' of
          Left rootLoc -> rootLoc
          Right m      -> refLocFromMappings m

refLocFromMappings :: LocationMappings a -> Loc
refLocFromMappings m = foldr f (LocalFile "") (allLocsInMappings m)
  where
    f a@(S3Obj{}) _ = a
    f _ b@(S3Obj{}) = b
    f a _           = a
