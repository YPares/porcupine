{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}


module System.TaskPipeline.ATask
  ( module Control.Category
  , module Control.Arrow
  , MonadThrow(..), TaskRunError(..)
  , ATask(..)
  , RscAccess(..)
  , RscAccessTree
  , IsTaskResource
  , rscAccessed
  , tryATask, throwATask, clockATask
  , unsafeLiftToATask
  , liftToATask
  , ataskInSubtree
  , voidTask
  ) where

import           Prelude                hiding (id, (.))

import           Control.Arrow
import           Control.Category
import           Control.DeepSeq        (NFData (..), force)
import           Control.Exception      (evaluate)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import qualified Data.Foldable          as F
import           Data.Locations
import           System.Clock


-- | Monoid that tracks the number of times a resource has been accessed.
data RscAccess a = RscAccess Int a

-- | A lens to the data contained in 'RscAccess'
rscAccessed :: Lens (RscAccess a) (RscAccess b) a b
rscAccessed f (RscAccess n x) = RscAccess n <$> f x

instance (Semigroup a) => Semigroup (RscAccess a) where
  RscAccess x a <> RscAccess y b = RscAccess (x+y) (a<>b)

instance (Monoid a) => Monoid (RscAccess a) where
  mempty = RscAccess 0 mempty  -- So that mappend x mempty is still equal to x

-- | What a task needs to run, namely files to be accessed and options to be
-- overriden. The options are put in a LocationTree too (it will be needed for
-- composed models). The trees are kept separated to simplify code creating
-- ATasks
type RscAccessTree n = LocationTree (RscAccess n)

-- | A Task than turns @a@ into @b@. It runs in some monad @m@ so can access
-- locations described in a 'LocationTree' with nodes of type @n@. @n@ is some
-- type of resource required by the task.
data ATask m n a b = ATask
  { aTaskResourceTree :: LocationTree (n WithDefaultUsage)
    -- ^ The tree of all resources required by task. When two tasks are
    -- sequenced, their resource tree are merged. When @n@ is
    -- 'PipelineResource', this LocationTree is an 'UnboundResourceTree'.
  , aTaskPerform      :: (a, RscAccessTree (n LocLayers)) -> m (b, RscAccessTree (n LocLayers))
  -- ^ The action performed by the task. It is passed the final tree containing
  -- the actual resources bound to their definitive value, and should update
  -- this tree.
  }

-- | The type used within a task for the resources must be a Monoid
type IsTaskResource n = (Monoid (n WithDefaultUsage), Monoid (n LocLayers))

-- | a tasks that discards its inputs and returns ()
voidTask :: (Monad m, Monoid (n WithDefaultUsage), Monoid (n LocLayers)) => ATask m n a ()
voidTask = arr (const ())

-- | Turn an action into a ATask. BEWARE! The resulting 'ATask' will have NO
-- requirements, so if the action uses files or resources, they won't appear in
-- the LocationTree.
unsafeLiftToATask :: (Functor m, IsTaskResource n) => (a -> m b) -> ATask m n a b
unsafeLiftToATask f = ATask mempty (\(a,t) -> (,t) <$> f a)

instance (Monad m, IsTaskResource n) => Category (ATask m n) where
  id = ATask mempty pure
  ATask t2 fn2 . ATask t1 fn1 = ATask (t1 <> t2) (fn1 >=> fn2)

instance (Monad m, IsTaskResource n) => Arrow (ATask m n) where
  arr f = ATask mempty $ \(a,tt) -> pure (f a,tt)
  -- We implement (***) instead of first so that (***) won't use (.) which is
  -- sequential, while (***) must be parallel
  ATask t1 fn1 *** ATask t2 fn2 = ATask (t1 <> t2) $
    \((a,b),tt) -> combine <$> fn1 (a,tt) <*> fn2 (b,tt)
    where
      combine (a', tt1) (b', tt2) = ((a',b'), tt1 <> tt2)

-- -- | to allow us to write if then else in task arrows
--
-- -- Commented out for now because it isn't clear whether we want it.
-- Task pipelines should be the most statically inspectable as possible,
-- and if/else and cases go against that.
--
-- instance (Monad m, IsTaskResource n) => ArrowChoice (ATask m n)
--   where
--     left :: ATask m n b c -> ATask m n (Either b d) (Either c d)
--     left (ATask tree perf) = ATask tree f
--       where
--         -- f :: (Either b d, RscAccessTree (n LocLayers)) -> m (Either c d, RscAccessTree (n LocLayers))
--         f (Left l, rscTree)  = do (b, rscTree') <- perf (l, rscTree)
--                                   return (Left b, rscTree')
--         f (Right r, rscTree) = return (Right r, rscTree)

instance (Functor m) => Functor (ATask m n a) where
  fmap f (ATask tree fn) = ATask tree (fmap (\(a,tt) -> (f a,tt)) . fn)

instance (Applicative m, IsTaskResource n) => Applicative (ATask m n a) where
  pure a = ATask mempty $ \(_,tt) -> pure (a, tt)
  ATask t1 fn1 <*> ATask t2 fn2 = ATask (t1 <> t2) $
    \i -> combine <$> fn1 i <*> fn2 i
    where
      combine (fn1', tt1) (b, tt2) = (fn1' b, tt1 <> tt2)

-- | An error when running a pipeline of tasks
newtype TaskRunError = TaskRunError String
  deriving (Show)

instance Exception TaskRunError where
  displayException (TaskRunError s) = s

-- | Catches an error happening in a task. Leaves the tree intact if an error
-- occured.
tryATask
  :: (Exception e, MonadCatch m) => ATask m n a b -> ATask m n a (Either e b)
tryATask (ATask reqs fn) = ATask reqs $ \i@(_, tree) -> do
  res <- try $ fn i
  return $ case res of
    Left e           -> (Left e, tree)
    Right (r, tree') -> (Right r, tree')

-- | Fails the whole pipeline if an exception occured, or just continues as
-- normal
throwATask :: (Exception e, MonadThrow m, IsTaskResource n) => ATask m n (Either e b) b
throwATask = ATask mempty $ \(i, tree) -> do
  case i of
    Left e  -> throwM e
    Right r -> return (r, tree)

-- This orphan instance is necessary so clockATask may work over an 'Either
-- SomeException a'
instance NFData SomeException where
  rnf e = rnf $ displayException e

-- | Measures the time taken by an 'ATask'
clockATask
  :: (NFData b, MonadIO m) => ATask m n a b -> ATask m n a (b, TimeSpec)
clockATask (ATask reqs fn) = ATask reqs $ \i -> do
  start <- liftIO $ getTime Realtime
  (output, tree') <- fn i
  liftIO $ do
    output' <- evaluate $ force output
    end     <- getTime Realtime
    return ((output', diffTimeSpec end start), tree')

-- | Wraps in a task a function that needs to access some items present in a
-- subfolder of the 'LocationTree' and mark these accesses as done. This is the
-- main way to create an ATask
liftToATask
  :: (LocationMonad m, Traversable t, IsTaskResource n)
  => [LocationTreePathItem]  -- ^ Path to subfolder in 'LocationTree'
  -> t (LTPIAndSubtree (n WithDefaultUsage))    -- ^ Items of interest in the subfolder
  -> String  -- ^ A name for the task (for error messages)
  -> (i -> t (n LocLayers) -> m o)       -- ^ What to run with these items
  -> ATask m n i o           -- ^ The resulting ATask
liftToATask path filesToAccess taskName writeFn = ATask tree runAccess
  where
    tree = foldr (\pathItem subtree -> folderNode [ pathItem :/ subtree ])
                 (folderNode $ F.toList filesToAccess) path
    runAccess (input, rscTree) = do
      let mbSubtree = rscTree ^? atSubfolderRec path
      subtree <- case mbSubtree of
        Just s -> return s
        Nothing -> throwM $ TaskRunError $
          taskName ++ ": path '" ++ show path ++ "' not found in the LocationTree"
      nodeTags <- forM filesToAccess $ \(filePathItem :/ _) -> do
        case subtree ^? atSubfolder filePathItem . locTreeNodeTag of
          Nothing -> throwM $ TaskRunError $
            taskName ++ ": path '" ++ show filePathItem ++ "' not found in the LocationTree"
          Just (RscAccess _ tag) -> return tag
      output <- writeFn input nodeTags
      let incrAccess (RscAccess n x) = RscAccess (n+1) x
          subtree' = foldr (\(filePathItem :/ _) t ->
                              t & atSubfolder filePathItem . locTreeNodeTag %~ incrAccess)
                           subtree filesToAccess
          rscTree' = rscTree & atSubfolderRec path .~ subtree'
      return (output, rscTree')

-- | Makes the 'LocationTree' associated to the task deeper by adding extra
-- levels on top of it. This is to solve conflicts between tasks that have
-- 'LocationTree's that are identical (for instance input files for a model if
-- you want to solve several models, in which case you'd want for instance to
-- add an extra level at the root of the tree with the model name. See the
-- Simple example in simwork-projects).
ataskInSubtree
  :: (IsTaskResource n, Monad m)
  => [LocationTreePathItem] -> ATask m n a b -> ATask m n a b
ataskInSubtree path (ATask tree fn) = ATask tree' fn'
  where
    tree' = foldr (\pathItem rest -> folderNode [pathItem :/ rest]) tree path
    fn' (i, t) = do
      (o, t') <- fn (i, t^.atSubfolderRec path)
      return (o, t & atSubfolderRec path .~ t')

