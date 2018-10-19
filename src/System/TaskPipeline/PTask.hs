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
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}


module System.TaskPipeline.PTask
  ( module Control.Category
  , module Control.Arrow
  , MonadThrow(..)
  , PTask(..)
  , RscAccess(..)
  , RscAccessTree
  , rscAccessed
  , tryPTask, throwPTask, clockPTask
  , unsafeLiftToPTask, unsafeRunIOTask
  , liftToPTask
  , ptaskInSubtree
  , voidTask
  , addContextToTask
  , addNamespaceToTask
  ) where

import           Prelude                          hiding (id, (.))

import           Control.Arrow
import           Control.Category
import           Control.DeepSeq                  (NFData (..), force)
import           Control.Exception                (evaluate)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import qualified Data.Foldable                    as F
import           Data.Locations
import           Data.String                      (IsString (..))
import           Katip
import           System.Clock
import           System.TaskPipeline.ResourceTree


-- | Monoid that tracks the number of times a resource has been accessed.
data RscAccess a = RscAccess Int a
  deriving Functor

-- | A lens to the data contained in 'RscAccess'
rscAccessed :: Lens (RscAccess a) (RscAccess b) a b
rscAccessed f (RscAccess n x) = RscAccess n <$> f x

instance (Semigroup a) => Semigroup (RscAccess a) where
  RscAccess x a <> RscAccess y b = RscAccess (x+y) (a<>b)

instance (Monoid a) => Monoid (RscAccess a) where
  mempty = RscAccess 0 mempty  -- So that mappend x mempty is still equal to x

-- | What a task needs to run, ie. data to be accessed (read or written).
type RscAccessTree n = LocationTree (RscAccess n)

-- | A task is an Arrow than turns @a@ into @b@. It runs in some monad @m@.
-- Each 'PTask' will expose its requirements in terms of resource it wants to
-- access in the form of a resource tree (implemented as a 'LocationTree' of
-- 'VirtualFile's). These trees of requirements are aggregated when the tasks
-- are combined with each other, so once the full pipeline has been composed
-- (through Arrow composition), its 'pTaskResourceTree' will contain the
-- complete requirements of the pipeline.
--
-- At the time of running, 'pTaskPerform' will be given the resource tree where
-- VirtualFiles have been replaced by function to pull and write the data.
data PTask m a b = PTask
  { pTaskResourceTree :: LocationTree VirtualFileNode
    -- ^ The tree of all resources required by task. When two tasks are
    -- sequenced, their resource tree are merged.
  , pTaskPerform      :: (a, RscAccessTree (DataAccessNode m)) -> m (b, RscAccessTree (DataAccessNode m))
  -- ^ The action performed by the task. It is passed the final tree containing
  -- the actual resources bound to their definitive value, and should update
  -- this tree.
  }

-- | a tasks that discards its inputs and returns ()
voidTask :: (Monad m) => PTask m a ()
voidTask = arr (const ())

-- | Turn an action into a PTask. BEWARE! The resulting 'PTask' will have NO
-- requirements, so if the action uses files or resources, they won't appear in
-- the LocationTree.
unsafeLiftToPTask :: (Functor m) => (a -> m b) -> PTask m a b
unsafeLiftToPTask f = PTask mempty (\(a,t) -> (,t) <$> f a)

-- | Runs an IO action. IT MUST NOT BE PERFORMING READS OR WRITES.
unsafeRunIOTask
  :: (LocationMonad m)
  => (i -> IO o)
  -> PTask m i o
unsafeRunIOTask f = unsafeLiftToPTask (liftIO . f)

instance (Monad m) => Category (PTask m) where
  id = PTask mempty pure
  PTask t2 fn2 . PTask t1 fn1 = PTask (t1 <> t2) (fn1 >=> fn2)

instance (Monad m) => Arrow (PTask m) where
  arr f = PTask mempty $ \(a,tt) -> pure (f a,tt)
  -- We implement (***) instead of first so that (***) won't use (.) which is
  -- sequential, while (***) must be parallel
  PTask t1 fn1 *** PTask t2 fn2 = PTask (t1 <> t2) $
    \((a,b),tt) -> combine <$> fn1 (a,tt) <*> fn2 (b,tt)
    where
      combine (a', tt1) (b', tt2) = ((a',b'), tt1 <> tt2)

-- -- | to allow us to write if then else in task arrows
--
-- -- Commented out for now because it isn't clear whether we want it.
-- Task pipelines should be the most statically inspectable as possible,
-- and if/else and cases go against that.
--
-- instance (Monad m) => ArrowChoice (PTask m)
--   where
--     left :: PTask m n b c -> PTask m n (Either b d) (Either c d)
--     left (PTask tree perf) = PTask tree f
--       where
--         -- f :: (Either b d, RscAccessTree (n LocLayers)) -> m (Either c d, RscAccessTree (n LocLayers))
--         f (Left l, rscTree)  = do (b, rscTree') <- perf (l, rscTree)
--                                   return (Left b, rscTree')
--         f (Right r, rscTree) = return (Right r, rscTree)

instance (Functor m) => Functor (PTask m a) where
  fmap f (PTask tree fn) = PTask tree (fmap (\(a,tt) -> (f a,tt)) . fn)

instance (Applicative m) => Applicative (PTask m a) where
  pure a = PTask mempty $ \(_,tt) -> pure (a, tt)
  PTask t1 fn1 <*> PTask t2 fn2 = PTask (t1 <> t2) $
    \i -> combine <$> fn1 i <*> fn2 i
    where
      combine (fn1', tt1) (b, tt2) = (fn1' b, tt1 <> tt2)

-- | Catches an error happening in a task. Leaves the tree intact if an error
-- occured.
tryPTask
  :: (Exception e, MonadCatch m) => PTask m a b -> PTask m a (Either e b)
tryPTask (PTask reqs fn) = PTask reqs $ \i@(_, tree) -> do
  res <- try $ fn i
  return $ case res of
    Left e           -> (Left e, tree)
    Right (r, tree') -> (Right r, tree')

-- | Fails the whole pipeline if an exception occured, or just continues as
-- normal
throwPTask :: (Exception e, KatipContext m, MonadThrow m) => PTask m (Either e b) b
throwPTask = PTask mempty $ \(i, tree) -> do
  case i of
    Left e  -> throwWithPrefix $ displayException e
    Right r -> return (r, tree)

-- This orphan instance is necessary so clockPTask may work over an 'Either
-- SomeException a'
instance NFData SomeException where
  rnf e = rnf $ displayException e

-- | Measures the time taken by an 'PTask'
clockPTask
  :: (NFData b, MonadIO m) => PTask m a b -> PTask m a (b, TimeSpec)
clockPTask (PTask reqs fn) = PTask reqs $ \i -> do
  start <- liftIO $ getTime Realtime
  (output, tree') <- fn i
  liftIO $ do
    output' <- evaluate $ force output
    end     <- getTime Realtime
    return ((output', diffTimeSpec end start), tree')

-- | Wraps in a task a function that needs to access some items present in a
-- subfolder of the 'LocationTree' and mark these accesses as done. This is the
-- main low-level way to create a PTask, but beware, most of the time you
-- probably want the higher-level interface of
-- System.TaskPipeline.Tasks.LayeredAccess.
liftToPTask
  :: (LocationMonad m, KatipContext m, Traversable t)
  => [LocationTreePathItem]  -- ^ Path to subfolder in 'LocationTree'
  -> t (LTPIAndSubtree VirtualFileNode)    -- ^ Items of interest in the subfolder
  -> (i -> t (DataAccessNode m) -> m o)       -- ^ What to run with these items
  -> PTask m i o           -- ^ The resulting PTask
liftToPTask path filesToAccess writeFn = PTask tree runAccess
  where
    tree = foldr (\pathItem subtree -> folderNode [ pathItem :/ subtree ])
                 (folderNode $ F.toList filesToAccess) path
    runAccess (input, rscTree) = do
      let mbSubtree = rscTree ^? atSubfolderRec path
      subtree <- case mbSubtree of
        Just s -> return s
        Nothing -> throwWithPrefix $
          "path '" ++ show path ++ "' not found in the LocationTree"
      nodeTags <- forM filesToAccess $ \(filePathItem :/ _) -> do
        case subtree ^? atSubfolder filePathItem . locTreeNodeTag of
          Nothing -> throwWithPrefix $
            "path '" ++ show filePathItem ++ "' not found in the LocationTree"
          Just (RscAccess _ tag) -> return tag
      output <- writeFn input nodeTags
      let incrAccess (RscAccess n x) = RscAccess (n+1) x
          subtree' = foldr (\(filePathItem :/ _) t ->
                              t & atSubfolder filePathItem . locTreeNodeTag %~ incrAccess)
                           subtree filesToAccess
          rscTree' = rscTree & atSubfolderRec path .~ subtree'
      return (output, rscTree')

-- | Moves the 'LocationTree' associated to the task deeper in the final
-- tree. This can be used to solve conflicts between tasks that have
-- 'LocationTree's that are identical (for instance input files for a model if
-- you want to solve several models, in which case you'd want for instance to
-- add an extra level at the root of the tree with the model name).
ptaskInSubtree
  :: (Monad m)
  => [LocationTreePathItem] -> PTask m a b -> PTask m a b
ptaskInSubtree path (PTask tree fn) = PTask tree' fn'
  where
    tree' = foldr (\pathItem rest -> folderNode [pathItem :/ rest]) tree path
    fn' (i, t) = do
      (o, t') <- fn (i, t^.atSubfolderRec path)
      return (o, t & atSubfolderRec path .~ t')

-- | Adds some context that will be used at logging time. See 'katipAddContext'
addContextToTask
  :: (KatipContext m, LogItem i)
  => i              -- ^ The context
  -> PTask m a b  -- ^ The task to wrap
  -> PTask m a b
addContextToTask item (PTask tree fn) =
  PTask tree $ katipAddContext item . fn

-- | Adds a namespace to the task. See 'katipAddNamespace'
addNamespaceToTask
  :: (KatipContext m)
  => String        -- ^ The namespace. (Is IsString instance)
  -> PTask m a b -- ^ The task to wrap
  -> PTask m a b
addNamespaceToTask ns (PTask tree fn) =
  PTask tree $ katipAddNamespace (fromString ns) . fn
