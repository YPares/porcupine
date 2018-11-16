{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}

-- | This module exposes the 'PTask' arrow along with some low-level functions
-- to create and run a 'PTask'.

module System.TaskPipeline.PTask.Internal
  ( PTask
  , PTaskReaderState
  , ptrsKatipContext
  , ptrsKatipNamespace
  , ptrsDataAccessTree
  , splittedPTask
  , runnablePTaskState
  , makePTask
  , makePTask'
  , withDataAccessTree
  , withDataAccessTree'
  , toRunnable
  , runnableWithoutReqs
  ) where

import           Prelude                          hiding (id, (.))

import           Control.Arrow
import           Control.Arrow.AppArrow
import           Control.Arrow.Async
import           Control.Arrow.Free               (ArrowError)
import           Control.Category
import           Control.Funflow
import           Control.Lens
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer
import           Data.Default
import           Data.Locations.LocationTree
import           Data.Locations.LogAndErrors
import           Katip.Core                       (Namespace)
import           Katip.Monadic
import           System.TaskPipeline.ResourceTree


type ReqTree = LocationTree VirtualFileNode
type DataAccessTree m = LocationTree (DataAccessNode m)

data PTaskReaderState m = PTaskReaderState
  { _ptrsKatipContext   :: !LogContexts
  , _ptrsKatipNamespace :: !Namespace
  , _ptrsDataAccessTree :: !(DataAccessTree m) }

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
type EffectInFlow m = AsyncA (ReaderT (DataAccessTree m) m)

instance (KatipContext m) => ArrowFlow (EffectInFlow m) SomeException (PTask m) where
  step' props f = flowToPTask $ step' props f
  stepIO' props f = flowToPTask $ stepIO' props f
  external f = flowToPTask $ external f
  external' props f = flowToPTask $ external' props f
  -- wrap' transmits the Reader state of the PTask down to the flow:
  wrap' props (AsyncA rdrAct) = runnableWithoutReqs $
    withDataAccessTree' props $ \tree input ->
                                  runReaderT (rdrAct input) tree
  putInStore f = flowToPTask $ putInStore f
  getFromStore f = flowToPTask $ getFromStore f
  internalManipulateStore f = flowToPTask $ internalManipulateStore f

-- | At the 'RunnablePTask' level, access the DataAccessTree and run an action
withDataAccessTree' :: (KatipContext m)
                    => Properties a b -> (DataAccessTree m -> a -> m b) -> RunnablePTask m a b
withDataAccessTree' props f = AppArrow $ reader $ \ptrs ->
  wrap' props $ AsyncA $ \input ->
    localKatipContext (<> _ptrsKatipContext ptrs) $
      localKatipNamespace (const $ _ptrsKatipNamespace ptrs) $
        f (_ptrsDataAccessTree ptrs) input

-- | 'withDataAccessTree'' without caching.
withDataAccessTree :: (KatipContext m)
                   => (DataAccessTree m -> a -> m b) -> RunnablePTask m a b
withDataAccessTree = withDataAccessTree' def

-- | Wraps a 'RunnablePTask' into a 'PTask' that declares no requirements
runnableWithoutReqs :: RunnablePTask m a b -> PTask m a b
runnableWithoutReqs = PTask . appArrow

-- | An Iso to the requirements and the runnable part of a 'PTask'
splittedPTask :: Iso' (PTask m a b) (ReqTree, RunnablePTask m a b)
splittedPTask = iso to_ from_
  where
    to_ (PTask (AppArrow wrtrAct)) = swap $ runWriter wrtrAct
    from_ = PTask . AppArrow . writer . swap
    swap (a,b) = (b,a)

-- | Permits to apply a function to the state of a 'RunnablePTask' when in runs.
runnablePTaskState :: Setter' (RunnablePTask m a b) (PTaskReaderState m)
runnablePTaskState = lens unAppArrow (const AppArrow) . setting local

-- | Just a shortcut around 'withDataAccessTree' when you just need to run an
-- action without accessing the tree.
toRunnable :: (KatipContext m)
           => (a -> m b) -> RunnablePTask m a b
toRunnable = withDataAccessTree . const

-- | Makes a task from a tree of requirements and a function. The 'Properties'
-- indicate whether we can cache this task.
makePTask' :: (KatipContext m)
           => Properties a b
           -> LocationTree VirtualFileNode
           -> (DataAccessTree m -> a -> m b)
           -> PTask m a b
makePTask' props tree f =
  (tree, withDataAccessTree' props f) ^. from splittedPTask

-- | Makes a task from a tree of requirements and a function. This is the entry
-- point to PTasks
makePTask :: (KatipContext m)
          => LocationTree VirtualFileNode
          -> (DataAccessTree m -> a -> m b)
          -> PTask m a b
makePTask = makePTask' def

