{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module System.TaskPipeline.PTask.Internal where

import           Control.Arrow
import           Control.Arrow.AppArrow
import           Control.Arrow.Async
import           Control.Arrow.Free               (ArrowError)
import           Control.Category
import           Control.Funflow
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer
import           Data.Locations.LocationTree
import           Data.Locations.LogAndErrors
import           Katip.Core                       (Namespace)
import           Katip.Monadic
import           System.TaskPipeline.ResourceTree

import           Prelude                          hiding (id, (.))


type ReqTree = LocationTree VirtualFileNode
type RunTree m = LocationTree (DataAccessNode m)

data PTaskReaderState m = PTaskReaderState
  { ptrsKatipContext   :: !LogContexts
  , ptrsKatipNamespace :: !Namespace
  , ptrsRunTree        :: !(RunTree m) }

type EffectInFlow m = AsyncA (ReaderT (RunTree m) m)

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
    (AppArrow
      (Reader (PTaskReaderState m)) -- The reader layer contains the mapped
                                     -- tree. Will be used only as an
                                     -- applicative too.
      (Flow (AsyncA m) TaskRunError))
    a b)
  deriving (Category, Arrow, ArrowError TaskRunError)

flowToPTask :: Flow (AsyncA m) TaskRunError a b -> PTask m a b
flowToPTask = PTask . appArrow . appArrow

instance (KatipContext m) => ArrowFlow (EffectInFlow m) TaskRunError (PTask m) where
  step' props f = flowToPTask $ step' props f
  stepIO' props f = flowToPTask $ stepIO' props f
  external f = flowToPTask $ external f
  external' props f = flowToPTask $ external' props f
  -- wrap' transmits the Reader state of the PTask down to the flow:
  wrap' props (AsyncA rdrAct) = PTask $ AppArrow $ pure $ AppArrow $ reader $ \ptrs ->
    wrap' props $ AsyncA $ \input ->
      localKatipContext (<> ptrsKatipContext ptrs) $
        localKatipNamespace (const $ ptrsKatipNamespace ptrs) $
          runReaderT (rdrAct input) $ ptrsRunTree ptrs
  putInStore f = flowToPTask $ putInStore f
  getFromStore f = flowToPTask $ getFromStore f
  internalManipulateStore f = flowToPTask $ internalManipulateStore f
