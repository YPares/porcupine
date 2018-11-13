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
  , PTask(..)
  , Severity(..)
  , tryPTask, throwPTask, clockPTask
  , unsafeLiftToPTask, unsafeRunIOTask
  , liftToPTask
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
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Foldable                      as F
import           Data.Locations
import           Katip
import           System.Clock
import           System.TaskPipeline.PTask.Internal
import           System.TaskPipeline.ResourceTree


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

-- | Wraps in a task a function that needs to access some items present in a
-- subfolder of the 'LocationTree' and mark these accesses as done. This is the
-- main low-level way to create a PTask, but beware, most of the time you
-- probably want the higher-level interface of
-- System.TaskPipeline.Tasks.LayeredAccess.
liftToPTask
  :: (MonadThrow m, KatipContext m, Traversable t)
  => [LocationTreePathItem]  -- ^ Path to subfolder in 'LocationTree'
  -> t (LTPIAndSubtree VirtualFileNode)    -- ^ Items of interest in the subfolder
  -> (i -> t (DataAccessNode m) -> m o)       -- ^ What to run with these items
  -> PTask m i o           -- ^ The resulting PTask
liftToPTask path filesToAccess writeFn =
  (tree, withDataAccessTree runAccess) ^. from splittedPTask
  where
    tree = foldr (\pathItem subtree -> folderNode [ pathItem :/ subtree ])
                 (folderNode $ F.toList filesToAccess) path
    runAccess rscTree input = do
      let mbSubtree = rscTree ^? atSubfolderRec path
      subtree <- case mbSubtree of
        Just s -> return s
        Nothing -> throwWithPrefix $
          "path '" ++ show path ++ "' not found in the LocationTree"
      nodeTags <- forM filesToAccess $ \(filePathItem :/ _) -> do
        case subtree ^? atSubfolder filePathItem . locTreeNodeTag of
          Nothing -> throwWithPrefix $
            "path '" ++ show filePathItem ++ "' not found in the LocationTree"
          Just tag -> return tag
      writeFn input nodeTags
