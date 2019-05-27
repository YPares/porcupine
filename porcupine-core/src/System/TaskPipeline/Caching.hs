{-# LANGUAGE Arrows        #-}
{-# LANGUAGE TupleSections #-}

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
import           Data.Aeson
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
  => Properties (a, [Value]) ()  -- ^ Location types aren't ContentHashable, but
                                 -- all are convertible to JSON. We need that to
                                 -- hash on locations so the task is repeated if
                                 -- we bind to new locations.
  -> VirtualFile b ignored       -- ^ The VirtualFile to write
  -> (a -> m b)                  -- ^ The function to lift.
  -> PTask m a ()
unsafeLiftToPTaskAndWrite_ props vf f =
  unsafeLiftToPTaskAndWrite props id vf $ fmap (,()) . f
{-# INLINE unsafeLiftToPTaskAndWrite_ #-}


-- | Similar to unsafeLiftToPTask', but caches a write action of the result
-- too. In this case we use the filepath bound to the VirtualFile to compute the
-- hash. That means that if the VirtualFile is bound to something else, the step
-- will be re-executed.
unsafeLiftToPTaskAndWrite
  :: (LogCatch m, Typeable b, Typeable ignored)
  => Properties (a', [Value]) c  -- ^ Location types aren't ContentHashable, but
                                 -- all are convertible to JSON. We need that to
                                 -- hash on locations so the task is repeated if
                                 -- we bind to new locations.
  -> (a -> a')                   -- ^ If the input mustn't or cannot be fully
                                 -- hashed, you can select a subset of it or
                                 -- transform it into a hashable intermediate
                                 -- representation (like aeson Value). Else just
                                 -- use 'id'
  -> VirtualFile b ignored       -- ^ The VirtualFile to write
  -> (a -> m (b,c))              -- ^ The function to lift. First item of the
                                 -- returned tuple will be written to the
                                 -- VirtualFile. The second will be returned by
                                 -- the task, so it must be loadable from the
                                 -- store.
  -> PTask m a c
unsafeLiftToPTaskAndWrite props inputHashablePart vf action = proc input -> do
  accessor <- getVFileDataAccessor [ATWrite] vf -< ()
  locs <- throwStringPTask -< daLocsAccessed accessor
  throwPTask <<< unsafeLiftToPTask' props' cached -< (input,map locToJ locs,accessor)
  where
    locToJ :: SomeLoc m -> Value
    locToJ (SomeGLoc l) = toJSON l

    cached (input,_,accessor) = do
      res <- SE.try $ action input
      case res of
        Right (outputForVFile, outputForStore) -> do
          daPerformWrite accessor outputForVFile
          return $ Right outputForStore
        Left err -> return $ Left (err::SomeException)

    props' = props { cache = cache'
                   , mdpolicy = updMdw <$> mdpolicy props }
    getH (input,locs,_) = (inputHashablePart input, locs)
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
