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
  , tryPTask, throwPTask, clockPTask
  , catchAndLog, throwStringPTask
  , ptaskOnJust, ptaskOnRight
  , unsafeLiftToPTask, unsafeLiftToPTask', unsafeRunIOTask
  , ptaskUsedFiles
  , ptaskRequirements
  , ptaskRunnablePart
  , ptaskDataAccessTree
  , ptaskInSubtree
  , voidTask
  , addContextToTask
  , addNamespaceToTask
  , logTask
  ) where

import           Prelude                            hiding (id, (.))

import           Control.Arrow
import qualified Control.Arrow.Free                 as AF
import           Control.Category
import           Control.DeepSeq                    (NFData (..), force)
import           Control.Exception                  (evaluate)
import           Control.Funflow                    (Properties)
import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Locations
import           Data.Locations.LogAndErrors
import           Data.String
import           Katip
import           System.Clock
import           System.TaskPipeline.PTask.Internal
import           System.TaskPipeline.ResourceTree


-- | a tasks that discards its inputs and returns ()
voidTask :: PTask m a ()
voidTask = arr (const ())

-- | Runs an IO action. IT MUST NOT BE PERFORMING READS OR WRITES.
unsafeRunIOTask
  :: (KatipContext m)
  => (i -> IO o)
  -> PTask m i o
unsafeRunIOTask f = unsafeLiftToPTask (liftIO . f)
                    -- TODO: implement it using stepIO instead

-- | Catches an error happening in a task. Leaves the tree intact if an error
-- occured.
tryPTask
  :: PTask m a b -> PTask m a (Either SomeException b)
tryPTask = AF.try

-- | An version of 'tryPTask' that just logs when an error happens
catchAndLog :: (KatipContext m)
            => Severity -> PTask m a b -> PTask m a (Maybe b)
catchAndLog severity task =
  tryPTask task
  >>> unsafeLiftToPTask (\i ->
        case i of
          Left e -> do
            logFM severity $ logStr $ displayException (e::SomeException)
            return Nothing
          Right x -> return $ Just x)

-- | Fails the whole pipeline if an exception occured, or just continues as
-- normal
throwPTask :: (Exception e, LogThrow m) => PTask m (Either e b) b
throwPTask = arr (over _Left displayException) >>> throwStringPTask

-- | Fails the whole pipeline if an exception occured, or just continues as
-- normal
throwStringPTask :: (LogThrow m) => PTask m (Either String b) b
throwStringPTask = unsafeLiftToPTask $ \i ->
  case i of
    Left e  -> throwWithPrefix e
    Right r -> return r

-- | Runs a PTask only if its input is Just
ptaskOnJust :: PTask m a b -> PTask m (Maybe a) (Maybe b)
ptaskOnJust = over ptaskRunnablePart $ \run -> proc input ->
  case input of
    Nothing -> returnA -< Nothing
    Just x  -> arr Just <<< run -< x

-- | Runs a PTask only if its input is Right
ptaskOnRight :: PTask m a b -> PTask m (Either e a) (Either e b)
ptaskOnRight = over ptaskRunnablePart $ \run -> proc input ->
  case input of
    Left e  -> returnA -< Left e
    Right x -> arr Right <<< run -< x

-- | Turn an action into a PTask. BEWARE! The resulting 'PTask' will have NO
-- requirements, so if the action uses files or resources, they won't appear in
-- the LocationTree.
unsafeLiftToPTask :: (KatipContext m)
                  => (a -> m b) -> PTask m a b
unsafeLiftToPTask = makePTask mempty . const

-- | A version of 'unsafeLiftToPTask' that can perform caching. It's analog to
-- funflow wrap' except the action passed here is just a simple function (it
-- will be wrapped later as a funflow effect).
unsafeLiftToPTask' :: (KatipContext m)
                   => Properties a b -> (a -> m b) -> PTask m a b
unsafeLiftToPTask' props = makePTask' props mempty . const

-- This orphan instance is necessary so clockPTask may work over an 'Either
-- SomeException a'
instance NFData SomeException where
  rnf e = rnf $ displayException e

-- | Measures the time taken by a 'PTask'
clockPTask
  :: (NFData b, KatipContext m) => PTask m a b -> PTask m a (b, TimeSpec)
clockPTask task = proc input -> do
  start <- unsafeRunIOTask $ const $ getTime Realtime -< ()
  output <- task -< input
  (output', end) <- unsafeRunIOTask timeEnd -< output
  returnA -< (output', end `diffTimeSpec` start)
  where timeEnd x = do
          x' <- evaluate $ force x
          (x',) <$> getTime Realtime

-- | Logs a message during the pipeline execution
logTask :: (KatipContext m) => PTask m (Severity, String) ()
logTask = unsafeLiftToPTask $ \(sev, s) -> logFM sev $ logStr s

-- | To access and transform the requirements of the PTask before it runs
ptaskRequirements :: Lens' (PTask m a b) (LocationTree VirtualFileNode)
ptaskRequirements = splittedPTask . _1

-- | To access and transform all the 'VirtualFiles' used by this 'PTask'. The
-- parameters of the VirtualFiles will remain hidden, but all the metadata is
-- accessible. NOTE: The original path of the files isn't settable.
ptaskUsedFiles :: Traversal' (PTask m a b) (VirtualFile Void ())
ptaskUsedFiles = ptaskRequirements . traversed . vfnodeFileVoided

-- | Permits to access the 'RunnablePTask' inside the PTask. It is the PTask,
-- devoid of its requirements. It is also and Arrow, and additionally it's an
-- ArrowChoice, so by using 'over ptaskRunnablePart' you can access a structure
-- in which you can use /case/ and /if/ statements.
ptaskRunnablePart :: Lens (PTask m a b) (PTask m a' b')
                     (RunnablePTask m a b) (RunnablePTask m a' b')
ptaskRunnablePart = splittedPTask . _2

-- | To transform the state of the PTask when it will run
ptaskReaderState :: Setter' (PTask m a b) (PTaskState m)
ptaskReaderState = ptaskRunnablePart . runnablePTaskState

-- | To transform the 'DataAccessTree' of the PTask when it will run
ptaskDataAccessTree :: Setter' (PTask m a b) (LocationTree (DataAccessNode m))
ptaskDataAccessTree = ptaskReaderState . ptrsDataAccessTree

-- | Adds some context that will be used at logging time. See 'katipAddContext'
addContextToTask :: (LogItem i) => i -> PTask m a b -> PTask m a b
addContextToTask item =
  over (ptaskReaderState . ptrsKatipContext) (<> (liftPayload item))

-- | Adds a namespace to the task. See 'katipAddNamespace'
addNamespaceToTask :: String -> PTask m a b -> PTask m a b
addNamespaceToTask ns =
  over (ptaskReaderState . ptrsKatipNamespace) (<> (fromString ns))

-- | Moves the 'LocationTree' associated to the task deeper in the final
-- tree. This can be used to solve conflicts between tasks that have
-- 'LocationTree's that are identical (for instance input files for a model if
-- you want to solve several models, in which case you'd want for instance to
-- add an extra level at the root of the tree with the model name).
ptaskInSubtree :: [LocationTreePathItem] -> PTask m a b -> PTask m a b
ptaskInSubtree path = over splittedPTask $ \(reqTree, runnable) ->
  let reqTree' = foldr (\pathItem rest -> folderNode [pathItem :/ rest]) reqTree path
      runnable' = runnable & over (runnablePTaskState . ptrsDataAccessTree)
                                  (view $ atSubfolderRec path)
  in (reqTree', runnable')
