{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE Arrows                     #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}


module System.TaskPipeline.PTask
  ( module Control.Category
  , module Control.Arrow
  , MonadThrow(..)
  , PTask
  , Severity(..)
  , tryPTask, throwPTask, clockPTask
  , unsafeLiftToPTask, unsafeRunIOTask
  , ptaskRequirements
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
import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Locations
import           Data.String
import           Katip
import           System.Clock
import           System.TaskPipeline.PTask.Internal
import           System.TaskPipeline.ResourceTree   (VirtualFileNode
                                                    ,DataAccessNode)


-- | a tasks that discards its inputs and returns ()
voidTask :: (Monad m) => PTask m a ()
voidTask = arr (const ())

-- | Runs an IO action. IT MUST NOT BE PERFORMING READS OR WRITES.
unsafeRunIOTask
  :: (KatipContext m)
  => (i -> IO o)
  -> PTask m i o
unsafeRunIOTask f = unsafeLiftToPTask (liftIO . f)

-- instance (Functor m) => Functor (PTask m a) where
--   fmap f (PTask tree fn) = PTask tree (fmap (\(a,tt) -> (f a,tt)) . fn)

-- instance (Applicative m) => Applicative (PTask m a) where
--   pure a = PTask mempty $ \(_,tt) -> pure (a, tt)
--   PTask t1 fn1 <*> PTask t2 fn2 = PTask (t1 <> t2) $
--     \i -> combine <$> fn1 i <*> fn2 i
--     where
--       combine (fn1', tt1) (b, tt2) = (fn1' b, tt1 <> tt2)

-- | Catches an error happening in a task. Leaves the tree intact if an error
-- occured.
tryPTask
  :: PTask m a b -> PTask m a (Either SomeException b)
tryPTask = AF.try

-- | Fails the whole pipeline if an exception occured, or just continues as
-- normal
throwPTask :: (Exception e, KatipContext m, MonadThrow m) => PTask m (Either e b) b
throwPTask = unsafeLiftToPTask $ \i ->
  case i of
    Left e  -> throwWithPrefix $ displayException e
    Right r -> return r

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

-- | To transform the 'DataAccessTree' of the PTask when it will run
ptaskDataAccessTree :: Setter' (PTask m a b) (LocationTree (DataAccessNode m))
ptaskDataAccessTree = ptaskReaderState . ptrsDataAccessTree


-- | To transform the state of the PTask when it will run
ptaskReaderState :: Setter' (PTask m a b) (PTaskReaderState m)
ptaskReaderState = splittedPTask . _2 . runnablePTaskState

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
