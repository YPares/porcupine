{-# LANGUAGE Arrows #-}

module System.TaskPipeline.Caching
  ( cacheWithVFile

  -- * Re-exports

  , Properties(..)
  , defaultCacherWithIdent
  , Default(..)
  ) where

import           Control.Funflow
import           Control.Funflow.ContentHashable
import           Control.Lens                          (over, traversed)
import           Control.Monad.Catch
import           Data.Default                          (Default (..))
import           Data.Locations.Loc
import           Data.Locations.LogAndErrors
import           Data.Locations.VirtualFile
import qualified Data.Text                             as T
import           Katip
import qualified Path
import           System.TaskPipeline.PTask
import           System.TaskPipeline.ResourceTree
import           System.TaskPipeline.VirtualFileAccess

import           Prelude                               hiding (id, (.))


-- | Similar to 'wrap' from 'ArrowFlow', but caches a part of the result
-- _outside_ of the store. In this case we use the filepath bound to the
-- VirtualFile to compute the hash. That means that if the VirtualFile is bound
-- to something else, the step will be re-executed.
cacheWithVFile :: (MonadThrow m, KatipContext m, Typeable c, Typeable c')
               => Properties (a', [Loc_ T.Text]) b  -- String isn't ContentHashable
               -> (a -> a')
               -> VirtualFile c c'
               -> (a -> m (b,c))
               -> PTask m a (b,c')
cacheWithVFile props inputHashablePart vf action = proc input -> do
  (locs,accessor) <-
    withVFileAccessFunction [ATWrite, ATRead] vf getLocsAndAccessor -< ()
  output <- unsafeLiftToPTask' props' cached -< (input,locs,accessor)
  fromVFile <- unsafeLiftToPTask daPerformRead -< accessor
  returnA -< (output, fromVFile)
  where
    getLocsAndAccessor getAccessor _ = do
      let accessor = getAccessor mempty
      locs <- case daLocsAccessed accessor of
        Left e  -> throwWithPrefix $
          "cacheWithVFile (" ++ showVFilePath vf ++ "): " ++ e
        Right r -> return r
      return (map (over traversed T.pack) locs, accessor)

    cached (input,_,accessor) = do
      (outputForStore, outputForVFile) <- action input
      daPerformWrite accessor outputForVFile
      return outputForStore

    props' = props { cache = cache'
                   , mdpolicy = updMdw <$> mdpolicy props }
    getH (input,locs,_) = (inputHashablePart input, locs)
    cache' = case cache props of
      NoCache -> NoCache
      Cache key sv rv ->
        let key' salt = key salt . getH
        in Cache key' sv rv
    updMdw mdWriter = mdWriter . getH
