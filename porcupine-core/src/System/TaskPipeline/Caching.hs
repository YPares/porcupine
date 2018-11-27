{-# LANGUAGE Arrows #-}

module System.TaskPipeline.Caching
  ( cacheWithVFile
  ) where

import Control.Funflow
import Control.Monad.Catch
import Data.Locations.VirtualFile
import Katip
import System.TaskPipeline.PTask.Internal
import System.TaskPipeline.VirtualFileAccess


-- | Similar to 'wrap' from 'ArrowFlow', but caches a part of the result
-- _outside_ of the store. In this case we use the filepath bound to the
-- VirtualFile to compute the hash. That means that if the VirtualFile is bound
-- to something else, the step will be re-executed.
cacheWithVFile :: (MonadThrow m, KatipContext m, Typeable c, Typeable c', Monoid c')
               => Properties a b
               -> VirtualFile c c'
               -> (a -> m (b,c))
               -> PTask m a (b,c')
cacheWithVFile props vf f = proc input -> do
  accessFn <- withVFileAccessFunction vf (\fn _ -> return $ fn mempty) -< ()
  -- makePTask' props (\_ ->
  undefined -< ()
