module System.TaskPipeline.Caching
  ( cacheWithVirtualFile
  ) where

import Control.Funflow
import Data.Locations.VirtualFile
import System.TaskPipeline.PTask.Internal
import System.TaskPipeline.VirtualFileAccess


-- | Similar to 'wrap' from 'ArrowFlow', but caches a part of the result
-- _outside_ of the store. In this case we use the filepath bound to the
-- VirtualFile to compute the hash. That means that if the VirtualFile is bound
-- to something else, the step will be re-executed.
cacheWithVirtualFile :: Properties a b -> VirtualFile c c' -> (a -> m (b,c)) -> PTask m a (b, c')
cacheWithVirtualFile props vf f = undefined
