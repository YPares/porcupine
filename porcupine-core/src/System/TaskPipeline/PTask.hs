{-# LANGUAGE Arrows               #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}


module System.TaskPipeline.PTask
  ( module Control.Category
  , module Control.Arrow
  , module Data.Locations.LogAndErrors
  , PTask
  , Severity(..)
  , CanRunPTask
  , Properties
  , tryTask, throwTask, clockTask, clockTask'
  , catchAndLog, throwStringTask
  , taskOnJust, taskOnRight
  , toTask, toTask'
  , ioTask, stepIO, stepIO'
  , taskUsedFiles
  , taskRequirements
  , taskRunnablePart
  , taskDataAccessTree
  , taskInSubtree
  , voidTask
  , addContextToTask
  , addStaticContextToTask
  , addNamespaceToTask
  , nameTask
  , logTask
  , logDebug, logInfo, logNotice, logWarning, logError
  ) where

import           Prelude                            hiding (id, (.))

import           Control.Arrow
import qualified Control.Arrow.Free                 as AF
import           Control.Category
import           Control.DeepSeq                    (NFData (..), force)
import           Control.Exception                  (evaluate)
import           Control.Funflow                    (Properties, stepIO, stepIO')
import           Control.Lens
import           Data.Locations
import           Data.Locations.LogAndErrors
import           Data.String
import           Katip
import           System.ClockHelpers
import           System.TaskPipeline.PorcupineTree
import           System.TaskPipeline.PTask.Internal


-- | a tasks that discards its inputs and returns ()
voidTask :: PTask m a ()
voidTask = arr (const ())

-- | Just a shortcut for when you want an IO step that requires no input
ioTask :: (KatipContext m) => PTask m (IO a) a
ioTask = stepIO id

-- | Catches an error happening in a task. Leaves the tree intact if an error
-- occured.
tryTask
  :: PTask m a b -> PTask m a (Either SomeException b)
tryTask = AF.try

-- | An version of 'tryPTask' that just logs when an error happens
catchAndLog :: (KatipContext m)
            => Severity -> PTask m a b -> PTask m a (Maybe b)
catchAndLog severity task =
  tryTask task
  >>> toTask (\i ->
        case i of
          Left e -> do
            logFM severity $ logStr $ displayException (e::SomeException)
            return Nothing
          Right x -> return $ Just x)

-- | Fails the whole pipeline if an exception occured, or just continues as
-- normal
throwTask :: (Exception e, LogThrow m) => PTask m (Either e b) b
throwTask = arr (over _Left displayException) >>> throwStringTask

-- | Fails the whole pipeline if an exception occured, or just continues as
-- normal
throwStringTask :: (LogThrow m) => PTask m (Either String b) b
throwStringTask = toTask $ \i ->
  case i of
    Left e  -> throwWithPrefix e
    Right r -> return r

-- | Runs a PTask only if its input is Just
taskOnJust :: PTask m a b -> PTask m (Maybe a) (Maybe b)
taskOnJust = over taskRunnablePart $ \run -> proc input ->
  case input of
    Nothing -> returnA -< Nothing
    Just x  -> arr Just <<< run -< x

-- | Runs a PTask only if its input is Right
taskOnRight :: PTask m a b -> PTask m (Either e a) (Either e b)
taskOnRight = over taskRunnablePart $ \run -> proc input ->
  case input of
    Left e  -> returnA -< Left e
    Right x -> arr Right <<< run -< x

-- | Turn an action into a PTask. BEWARE! The resulting 'PTask' will have NO
-- requirements, so if the action uses files or resources, they won't appear in
-- the LocationTree.
toTask :: (KatipContext m)
        => (a -> m b) -> PTask m a b
toTask = makeTask mempty . const

-- | A version of 'toTask' that can perform caching. It's analog to
-- funflow wrap' except the action passed here is just a simple function (it
-- will be wrapped later as a funflow effect).
toTask' :: (KatipContext m)
         => Properties a b -> (a -> m b) -> PTask m a b
toTask' props = makeTask' props mempty . const


-- This orphan instance is necessary so clockTask may work over an 'Either
-- SomeException a'
instance NFData SomeException where
  rnf e = rnf $ displayException e

-- | Measures the time taken by a 'PTask'.
clockTask
  :: (KatipContext m) => PTask m a b -> PTask m a (b, TimeSpec)
clockTask task = proc input -> do
  start <- time -< ()
  output <- task -< input
  end <- time -< ()
  returnA -< (output, end `diffTimeSpec` start)
  where
    time = stepIO $ const $ getTime Realtime

-- | Measures the time taken by a 'PTask' and the deep evaluation of its result.
clockTask'
  :: (NFData b, KatipContext m) => PTask m a b -> PTask m a (b, TimeSpec)
clockTask' task = clockTask $
  task >>> stepIO (evaluate . force)

-- | Logs a message during the pipeline execution
logTask :: (KatipContext m) => PTask m (Severity, String) ()
logTask = toTask $ \(sev, s) -> logFM sev $ logStr s

-- | Logs a message at a predefined severity level
logDebug, logInfo, logNotice, logWarning, logError :: (KatipContext m) => PTask m String ()
logDebug = arr (DebugS,) >>> logTask
logInfo = arr (InfoS,) >>> logTask
logNotice = arr (NoticeS,) >>> logTask
logWarning = arr (WarningS,) >>> logTask
logError = arr (ErrorS,) >>> logTask

-- | To access and transform the requirements of the PTask before it runs
taskRequirements :: Lens' (PTask m a b) (LocationTree VirtualFileNode)
taskRequirements = splitTask . _1

-- | To access and transform all the 'VirtualFiles' used by this 'PTask'. The
-- parameters of the VirtualFiles will remain hidden, but all the metadata is
-- accessible. NOTE: The original path of the files isn't settable.
taskUsedFiles :: Traversal' (PTask m a b) (VirtualFile NoWrite NoRead)
taskUsedFiles = taskRequirements . traversed . vfnodeFileVoided

-- | Permits to access the 'RunnableTask' inside the PTask. It is the PTask,
-- devoid of its requirements. It is also and Arrow, and additionally it's an
-- ArrowChoice, so by using 'over ptaskRunnablePart' you can access a structure
-- in which you can use /case/ and /if/ statements.
taskRunnablePart :: Lens (PTask m a b) (PTask m a' b')
                    (RunnableTask m a b) (RunnableTask m a' b')
taskRunnablePart = splitTask . _2

-- | To transform the state of the PTask when it will run
taskReaderState :: Setter' (PTask m a b) (PTaskState m)
taskReaderState = taskRunnablePart . runnableTaskReaderState

-- | To transform the 'DataAccessTree' of the PTask when it will run
taskDataAccessTree :: Setter' (PTask m a b) (LocationTree (DataAccessNode m))
taskDataAccessTree = taskReaderState . ptrsDataAccessTree

-- | Adds some context to a task, that will be used by the logger. That bit of
-- context is dynamic, that's why what we do is wrap the task into a new one,
-- expecting the 'LogItem'. See 'katipAddContext'. If your bit of context can be
-- known statically (ie. before the pipeline actually runs), prefer
-- 'addStaticContextToTask'.
addContextToTask :: (LogItem i, Monad m) => PTask m a b -> PTask m (i,a) b
addContextToTask = over taskRunnablePart $ modifyingRuntimeState
  (\(item,_) -> over ptrsKatipContext (<> liftPayload item))
  snd

-- | Adds to a task some context that is know _before_ the pipeline run. The
-- 'LogItem' to add is therefore static and can be given just as an argument.
addStaticContextToTask :: (LogItem i) => i -> PTask m a b -> PTask m a b
addStaticContextToTask item =
  over (taskReaderState . ptrsKatipContext) (<> liftPayload item)

-- | Adds a namespace to the task. See 'katipAddNamespace'. Like context in
-- 'addStaticContextToTask', the namespace is meant to be static, that's why we
-- give it as a parameter to 'addNamespaceToTask', instead of creating a PTask
-- that expects the namespace as an input.
--
-- NOTE: Prefer the use of 'nameTask', which records the time spent within the
-- task. Directly use 'addNamespaceToTask' only if that time tracking hurts
-- performance.
addNamespaceToTask :: String -> PTask m a b -> PTask m a b
addNamespaceToTask ns =
    over (taskReaderState . ptrsKatipNamespace) (<> fromString ns)

-- | This gives the task a name, making porcupine aware that this task should be
-- considered a entity by itself. This has a few effects:
--
-- change the logging output by wrapping it in a namespace (as per
-- 'addNamespaceToTask') and measure and log (InfoS level) the time spent within
-- that task
nameTask :: (KatipContext m) => String -> PTask m a b -> PTask m a b
nameTask ns task =
  addNamespaceToTask ns $
    clockTask task
    >>> toTask (\(output, time) -> do
          katipAddContext time $
            logFM InfoS $ logStr $ "Finished task '" ++ ns ++ "' in " ++ showTimeSpec time
          return output)

-- | Moves the 'LocationTree' associated to the task deeper in the final
-- tree. This can be used to solve conflicts between tasks that have
-- 'LocationTree's that are identical (for instance input files for a model if
-- you want to solve several models, in which case you'd want for instance to
-- add an extra level at the root of the tree with the model name).
taskInSubtree :: [LocationTreePathItem] -> PTask m a b -> PTask m a b
taskInSubtree path = over splitTask $ \(reqTree, runnable) ->
  let reqTree' = foldr (\pathItem rest -> folderNode [pathItem :/ rest]) reqTree path
      runnable' = runnable & over (runnableTaskReaderState . ptrsDataAccessTree)
                                  (view $ atSubfolderRec path)
  in (reqTree', runnable')
