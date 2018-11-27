{-# LANGUAGE Arrows #-}

module System.TaskPipeline.Caching
  ( cacheWithVFile
  ) where

import Control.Funflow
import Control.Monad.Catch
import Data.Locations.Loc
import Data.Locations.VirtualFile
import Katip
import System.TaskPipeline.PTask.Internal
import System.TaskPipeline.VirtualFileAccess
import System.TaskPipeline.ResourceTree
import Control.Funflow.ContentHashable
import qualified Path


-- | Similar to 'wrap' from 'ArrowFlow', but caches a part of the result
-- _outside_ of the store. In this case we use the filepath bound to the
-- VirtualFile to compute the hash. That means that if the VirtualFile is bound
-- to something else, the step will be re-executed.
cacheWithVFile :: (MonadThrow m, KatipContext m, Typeable c, Typeable ignored, Monoid ignored
                  )
               => Properties (a',[Loc]) b
               -> VirtualFile c ignored
               -> (a -> a')
               -> (a -> m (b,c))
               -> PTask m a b
cacheWithVFile props vf inputHashablePart action = proc input -> do
  (locs,accessFn) <- withVFileAccessFunction vf getLocsAndAccessFn -< ()
  makePTask' props' mempty cached -< (input,locs,accessFn)
  where
    cached _ (input, _, accessFn) = do
      (outputForStore, outputForVFile) <- action input
      accessFn outputForVFile
      return outputForStore

    getLocsAndAccessFn fn _ = do
      let da = fn mempty
      locs <- getLocsAccessed da
      return (locs, runDataAccessFn da)

    getH (input,locs,_) = (inputHashablePart input, locs)
    cache' = case cache props of
      NoCache -> NoCache
      Cache key sv rv ->
        let key' salt = key salt . getH
        in Cache key' sv rv
    updMdw mdWriter = mdWriter . getH
    props' = props { cache = cache'
                   , mdpolicy = updMdw <$> mdpolicy props }
