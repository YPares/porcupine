{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}

-- | This module exposes the 'PTask' arrow along with some low-level functions
-- to create and run a 'PTask'.

module System.TaskPipeline.PTask.Internal
  ( PTask
  , PTaskReaderState
  , RunnablePTask
  , FunflowRunConfig(..)
  , CanRunPTask
  , ptrsKatipContext
  , ptrsKatipNamespace
  , ptrsFunflowRunConfig
  , ptrsDataAccessTree
  , splittedPTask
  , runnablePTaskState
  , makePTask
  , makePTask'
  , withRunnableState
  , withRunnableState'
  , execRunnablePTask
  , toRunnable
  , runnableWithoutReqs
  , withPTaskReaderState
  ) where

import           Prelude                                     hiding (id, (.))

import           Control.Arrow
import           Control.Arrow.AppArrow
import           Control.Arrow.Async
import           Control.Arrow.Free                          (ArrowError)
import           Control.Category
import           Control.Funflow
import qualified Control.Funflow.ContentStore                as CS
import           Control.Funflow.External.Coordinator
import           Control.Funflow.External.Coordinator.SQLite
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer
import           Data.Default
import           Data.Locations.LocationTree
import           Data.Locations.LogAndErrors
import           Katip.Core                                  (Namespace)
import           Katip.Monadic
import           Path
import           System.TaskPipeline.ResourceTree


type ReqTree = LocationTree VirtualFileNode
type DataAccessTree m = LocationTree (DataAccessNode m)

-- | PTask functions like mappingOverStream make necessary to recursively run
-- some flows. Until we find a better solution than to run flows in flows, this
-- is how we do it. These are the arguments to
-- Control.Funflow.Exec.Simple.runFlowEx
data FunflowRunConfig = forall c. (Coordinator c) => FunflowRunConfig
  { _ffrcCoordinator       :: !c
  , _ffrcCoordinatorConfig :: !(Config c)
  , _ffrcContentStore      :: !CS.ContentStore
  , _ffrcFlowIdentity      :: !Int }

-- | This is the state that will be shared by the whole PTask pipeline once it
-- starts running.
data PTaskReaderState m = PTaskReaderState
  { _ptrsKatipContext     :: !LogContexts
  , _ptrsKatipNamespace   :: !Namespace
  , _ptrsFunflowRunConfig :: !FunflowRunConfig
  , _ptrsDataAccessTree   :: !(DataAccessTree m) }

makeLenses ''PTaskReaderState

-- | The part of a 'PTask' that will be ran once the whole pipeline is composed
-- and the tree of requirements has been bound to physical locations. Is is
-- important to note that while both 'PTask' and 'RunnablePTask' are Arrows,
-- only 'RunnablePTask' is an ArrowChoice.
type RunnablePTask m =
  AppArrow
    (Reader (PTaskReaderState m)) -- The reader layer contains the mapped
                                  -- tree. Will be used only as an applicative.
    (Flow (AsyncA m) SomeException)

-- | The constraints that must be satisfied by the base monad m so that a @PTask
-- m@ can be run
type CanRunPTask m = (MonadBaseControl IO m, MonadMask m, KatipContext m)

-- | Runs a 'RunnablePTask' given its state
execRunnablePTask
  :: (CanRunPTask m)
  => RunnablePTask m a b -> PTaskReaderState m -> a -> m b
execRunnablePTask (AppArrow act)
  st@(PTaskReaderState{_ptrsFunflowRunConfig=FunflowRunConfig{..}}) =
  runFlowEx _ffrcCoordinator _ffrcCoordinatorConfig
            _ffrcContentStore id _ffrcFlowIdentity $
    runReader act st

-- | A task is an Arrow than turns @a@ into @b@. It runs in some monad @m@.
-- Each 'PTask' will expose its requirements in terms of resource it wants to
-- access in the form of a resource tree (implemented as a 'LocationTree' of
-- 'VirtualFile's). These trees of requirements are aggregated when the tasks
-- are combined with each other, so once the full pipeline has been composed
-- (through Arrow composition), its 'pTaskResourceTree' will contain the
-- complete requirements of the pipeline.
newtype PTask m a b = PTask
  (AppArrow
    (Writer ReqTree)  -- The writer layer accumulates the requirements. It will
                      -- be used only as an applicative.
    (RunnablePTask m)
    a b)
  deriving (Category, Arrow, ArrowError SomeException)

flowToPTask :: Flow (AsyncA m) SomeException a b -> PTask m a b
flowToPTask = PTask . appArrow . appArrow

-- | The type of effects we can run. The reader layer is executed by 'wrap',
-- this is why it doesn't appear in the Flow part of the 'RunnablePTask' type.
type EffectInFlow m = AsyncA (ReaderT (PTaskReaderState m) m)

instance (KatipContext m) => ArrowFlow (EffectInFlow m) SomeException (PTask m) where
  step' props f = flowToPTask $ step' props f
  stepIO' props f = flowToPTask $ stepIO' props f
  external f = flowToPTask $ external f
  external' props f = flowToPTask $ external' props f
  -- wrap' transmits the Reader state of the PTask down to the flow:
  wrap' props (AsyncA rdrAct) = runnableWithoutReqs $
    withRunnableState' props $ \state input ->
                              runReaderT (rdrAct input) state
  putInStore f = flowToPTask $ putInStore f
  getFromStore f = flowToPTask $ getFromStore f
  internalManipulateStore f = flowToPTask $ internalManipulateStore f

-- | At the 'RunnablePTask' level, access the reader state and run an action
withRunnableState' :: (KatipContext m)
                   => Properties a b -> (PTaskReaderState m -> a -> m b) -> RunnablePTask m a b
withRunnableState' props f = AppArrow $ reader $ \ptrs ->
  wrap' props $ AsyncA $ \input ->
    localKatipContext (const $ _ptrsKatipContext ptrs) $
      localKatipNamespace (const $ _ptrsKatipNamespace ptrs) $
        f ptrs input

-- | 'withRunnableState'' without caching.
withRunnableState :: (KatipContext m)
                  => (PTaskReaderState m -> a -> m b) -> RunnablePTask m a b
withRunnableState = withRunnableState' def

-- | Wraps a 'RunnablePTask' into a 'PTask' that declares no requirements
runnableWithoutReqs :: RunnablePTask m a b -> PTask m a b
runnableWithoutReqs = PTask . appArrow

-- | An Iso to the requirements and the runnable part of a 'PTask'
splittedPTask :: Iso (PTask m a b) (PTask m a' b')
                     (ReqTree, RunnablePTask m a b)
                     (ReqTree, RunnablePTask m a' b')
splittedPTask = iso to_ from_
  where
    to_ (PTask (AppArrow wrtrAct)) = swap $ runWriter wrtrAct
    from_ = PTask . AppArrow . writer . swap
    swap (a,b) = (b,a)

-- | Permits to apply a function to the state of a 'RunnablePTask' when in runs.
runnablePTaskState :: Setter' (RunnablePTask m a b) (PTaskReaderState m)
runnablePTaskState = lens unAppArrow (const AppArrow) . setting local

-- | Just a shortcut around 'withRunnableState' when you just need to run an
-- action without accessing the tree.
toRunnable :: (KatipContext m)
           => (a -> m b) -> RunnablePTask m a b
toRunnable = withRunnableState . const

-- | Makes a task from a tree of requirements and a function. The 'Properties'
-- indicate whether we can cache this task.
makePTask' :: (KatipContext m)
           => Properties a b
           -> LocationTree VirtualFileNode
           -> (DataAccessTree m -> a -> m b)
           -> PTask m a b
makePTask' props tree f =
  (tree, withRunnableState' props (f . _ptrsDataAccessTree)) ^. from splittedPTask

-- | Makes a task from a tree of requirements and a function. This is the entry
-- point to PTasks
makePTask :: (KatipContext m)
          => LocationTree VirtualFileNode
          -> (DataAccessTree m -> a -> m b)
          -> PTask m a b
makePTask = makePTask' def


withFunflowRunConfig :: (MonadIO m, MonadMask m) => (FunflowRunConfig -> m r) -> m r
withFunflowRunConfig f = do
  CS.withStore [absdir|/tmp/_ffstore|] $ \store -> do
    f $ FunflowRunConfig SQLite [absdir|/tmp/_ffcoordinator.db|] store 23090341

-- | Given a 'KatipContext' and a 'DataAccessTree', gets the initial state to
-- give to 'execRunnablePTask'
withPTaskReaderState :: (MonadIO m, MonadMask m, KatipContext m)
                     => DataAccessTree m
                     -> (PTaskReaderState m -> m r) -> m r
withPTaskReaderState tree f = withFunflowRunConfig $ \ffconfig -> do
  ctx <- getKatipContext
  ns  <- getKatipNamespace
  f $ PTaskReaderState ctx ns ffconfig tree
