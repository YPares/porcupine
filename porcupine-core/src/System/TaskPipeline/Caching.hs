{-# LANGUAGE Arrows #-}

module System.TaskPipeline.Caching
  ( cacheWithVFile
  ) where

import Control.Funflow
import Control.Monad.Catch
import Control.Lens (over, traversed)
import Data.Locations.Loc
import Data.Locations.VirtualFile
import Katip
import System.TaskPipeline.PTask.Internal
import System.TaskPipeline.VirtualFileAccess
import System.TaskPipeline.ResourceTree
import Control.Funflow.ContentHashable
import qualified Path
import qualified Data.Text as T


-- | Similar to 'wrap' from 'ArrowFlow', but caches a part of the result
-- _outside_ of the store. In this case we use the filepath bound to the
-- VirtualFile to compute the hash. That means that if the VirtualFile is bound
-- to something else, the step will be re-executed.
cacheWithVFile :: (MonadThrow m, KatipContext m, Typeable c
                  ,Typeable ignored, Monoid ignored)
               => Properties (a', [Loc_ T.Text]) b  -- String isn't ContentHashable
               -> (a -> a')
               -> VirtualFile c ignored
               -> (a -> m (b,c))
               -> PTask m a b
cacheWithVFile props inputHashablePart vf action = proc input -> do
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
      return (map (over traversed T.pack) locs, runDataAccessFn da)

    props' = props { cache = cache'
                   , mdpolicy = updMdw <$> mdpolicy props }
    getH (input,locs,_) = (inputHashablePart input, locs)
    cache' = case cache props of
      NoCache -> NoCache
      Cache key sv rv ->
        let key' salt = key salt . getH
        in Cache key' sv rv
    updMdw mdWriter = mdWriter . getH
