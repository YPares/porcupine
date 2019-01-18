{-# LANGUAGE Arrows        #-}
{-# LANGUAGE TupleSections #-}

module System.TaskPipeline.Caching
  ( cacheWithVFile

  -- * Re-exports

  , module Data.Locations.LogAndErrors
  , Properties(..)
  , defaultCacherWithIdent
  , Default(..)
  ) where

import qualified Control.Exception.Safe                as SE
import           Control.Funflow
import           Data.Aeson
import           Data.Default                          (Default (..))
import           Data.Locations.LogAndErrors
import           Data.Locations.VirtualFile
import           System.TaskPipeline.PTask
import           System.TaskPipeline.ResourceTree
import           System.TaskPipeline.VirtualFileAccess

import           Prelude                               hiding (id, (.))


-- | Similar to 'wrap' from 'ArrowFlow', but caches a part of the result
-- _outside_ of the store. In this case we use the filepath bound to the
-- VirtualFile to compute the hash. That means that if the VirtualFile is bound
-- to something else, the step will be re-executed.
cacheWithVFile :: (LogCatch m, Typeable c, Typeable c')
               => Properties (a', [Value]) b  -- Locs aren't ContentHashable,
                                              -- but they are convertible to JSON
               -> (a -> a')
               -> VirtualFile c c'
               -> (a -> m (b,c))
               -> PTask m a (b,c')
cacheWithVFile props inputHashablePart vf action = proc input -> do
  (locs,accessor) <-
    withVFileInternalAccessFunction [ATWrite,ATRead] vf getLocsAndAccessor -< ()
  output <- unsafeLiftToPTask' props' cached -< (input,map toJ locs,accessor)
  unsafeLiftToPTask afterCached -< (output,accessor)
  where
    toJ :: SomeLoc m -> Value
    toJ (SomeGLoc l) = toJSON l

    getLocsAndAccessor getAccessor _ _ = do
      let accessor = getAccessor mempty
      locs <- case daLocsAccessed accessor of
        Left e  -> throwWithPrefix $
          "cacheWithVFile (" ++ showVFileOriginalPath vf ++ "): " ++ e
        Right r -> return r
      return (locs, accessor)

    cached (input,_,accessor) = do
      res <- SE.try $ action input
      case res of
        Right (outputForStore, outputForVFile) -> do
          daPerformWrite accessor outputForVFile
          return $ Right outputForStore
        Left err -> return $ Left (err::SomeException)

    afterCached (output,accessor) = do
      case output of
        Right o  -> (o,) <$> daPerformRead accessor
        Left err -> throwWithPrefix $ displayException err

    props' = props { cache = cache'
                   , mdpolicy = updMdw <$> mdpolicy props }
    getH (input,locs,_) = (inputHashablePart input, locs)
    cache' = case cache props of
      NoCache -> NoCache
      Cache key sv rv ->
        let key' salt = key salt . getH
            sv' (Left e) = error $
              "cacheWithVFile: An exception occured during the cached function: " ++ displayException e
            sv' (Right x) = sv x
            rv' = Right . rv
        in Cache key' sv' rv'
    updMdw mdWriter i (Right o) = mdWriter (getH i) o
    updMdw _        _ (Left  _) = []
