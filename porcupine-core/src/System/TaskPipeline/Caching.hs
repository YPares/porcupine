{-# LANGUAGE Arrows        #-}
{-# LANGUAGE TupleSections #-}

-- | Functions in that module are necessary only if you want a fine control over
-- the caching of some actions. When you want to perform several reads and
-- writes from and to VirtualFiles as part of a /single/ cached task, the recommended way is
-- to use:
--
-- - 'getVFileReader'/'getVFileWriter' to obtain the accessors
-- - 'toPTask'' to create the cached task, to which you give the accessors
--
-- Given the accessors are hashable, the files that are bound to them are
-- incorporated to the hash, so binding them to new files will re-trigger the
-- task.

module System.TaskPipeline.Caching
  ( unsafeLiftToPTaskAndWrite
  , unsafeLiftToPTaskAndWrite_

  -- * Re-exports

  , module Data.Locations.LogAndErrors
  , Properties(..)
  , defaultCacherWithIdent
  , Default(..)
  ) where

import qualified Control.Exception.Safe                as SE
import           Control.Funflow
import           Data.Default                          (Default (..))
import           Data.Locations.LogAndErrors
import           Data.Locations.VirtualFile
import           System.TaskPipeline.PTask
import           System.TaskPipeline.ResourceTree
import           System.TaskPipeline.VirtualFileAccess

import           Prelude                               hiding (id, (.))


-- | For when the result of the lifted function just needs to be written, not
-- returned.
unsafeLiftToPTaskAndWrite_
  :: (LogCatch m, Typeable b, Typeable ignored)
  => Properties (a, DataWriter m b) ()  -- ^ Location types aren't ContentHashable, but
                                 -- all are convertible to JSON. We need that to
                                 -- hash on locations so the task is repeated if
                                 -- we bind to new locations.
  -> VirtualFile b ignored       -- ^ The VirtualFile to write
  -> (a -> m b)                  -- ^ The function to lift. Won't be executed if
                                 -- the file isn't mapped
  -> PTask m a ()
unsafeLiftToPTaskAndWrite_ props vf f =
  unsafeLiftToPTaskAndWrite props id vf (fmap (,()) . f) (const $ return ())
{-# INLINE unsafeLiftToPTaskAndWrite_ #-}


-- | Similar to 'toPTask'', but caches a write action of the result too. In this
-- case we use the filepath bound to the VirtualFile to compute the hash. That
-- means that if the VirtualFile is bound to something else, the step will be
-- re-executed.
unsafeLiftToPTaskAndWrite
  :: (LogCatch m, Typeable b, Typeable ignored)
  => Properties (a', DataWriter m b) c  -- ^ Location types aren't ContentHashable, but
                                 -- all are convertible to JSON. We need that to
                                 -- hash on locations so the task is repeated if
                                 -- we bind to new locations.
  -> (a -> a')                   -- ^ If the input mustn't or cannot be fully
                                 -- hashed, you can select a subset of it or
                                 -- transform it into a hashable intermediate
                                 -- representation (like aeson Value). Else just
                                 -- use 'id'
  -> VirtualFile b ignored       -- ^ The VirtualFile to write. If the file
                                 -- isn't mapped, the action won't be performed,
                                 -- and the task will return the default result.
  -> (a -> m (b,c))              -- ^ The function to lift. First item of the
                                 -- returned tuple will be written to the
                                 -- VirtualFile. The second will be returned by
                                 -- the task, so it must be loadable from the
                                 -- store.
  -> (a -> m c)                  -- ^ Called when the VirtualFile isn't mapped,
                                 -- and therefore no @b@ needs to be computed
  -> PTask m a c
unsafeLiftToPTaskAndWrite props inputHashablePart vf action actionWhenNotMapped = proc input -> do
  writer <- getVFileWriter vf -< ()
  throwPTask <<< toPTask' props' cached -< (input,writer)
  where
    cached (input,writer) | null (dwLocsAccessed writer)
      = Right <$> actionWhenNotMapped input
                          | otherwise
      = do
      res <- SE.try $ action input
      case res of
        Right (outputForVFile, outputForStore) -> do
          dwPerformWrite writer outputForVFile
          return $ Right outputForStore
        Left err -> return $ Left (err::SomeException)

    props' = props { cache = cache'
                   , mdpolicy = updMdw <$> mdpolicy props }
    getH (input,writer) = (inputHashablePart input,writer)
    cache' = case cache props of
      NoCache -> NoCache
      Cache key sv rv ->
        let key' salt = key salt . getH
            sv' (Left e) = error $
              "unsafeLiftToPTaskAndWrite: An exception occured during the cached function: "
              ++ displayException e
            sv' (Right x) = sv x
            rv' = Right . rv
        in Cache key' sv' rv'
    updMdw mdWriter i (Right o) = mdWriter (getH i) o
    updMdw _        _ (Left  _) = []
