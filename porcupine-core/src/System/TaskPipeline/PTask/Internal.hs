{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import           System.TaskPipeline.ResourceTree


type ReqTree = LocationTree VirtualFileNode
type RunTree m = LocationTree (DataAccessNode m)
type EffectInFlow m = AsyncA (ReaderT (RunTree m) m)

-- | A task is an Arrow than turns @a@ into @b@. It runs in some monad @m@.
-- Each 'PTask' will expose its requirements in terms of resource it wants to
-- access in the form of a resource tree (implemented as a 'LocationTree' of
-- 'VirtualFile's). These trees of requirements are aggregated when the tasks
-- are combined with each other, so once the full pipeline has been composed
-- (through Arrow composition), its 'pTaskResourceTree' will contain the
-- complete requirements of the pipeline.
newtype PTask m a b =
  PTask (AppArrow
          (Writer ReqTree)  -- The writer layer accumulates the requirements
          (AppArrow
            (Reader (RunTree m))  -- The reader layer contains the mapped tree
            (Flow (EffectInFlow m) TaskRunError))
                  -- Internally, the flow layer also has effects that depend on
                  -- a reader layer. The two reader layers are synchronized by
                  -- 'wrap'
          a b)
  deriving (Category, Arrow, ArrowError TaskRunError)
