{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -Wall #-}

-- | This module provides some utilities for when the pipeline needs to access
-- several files organized in layers for each location in the 'LocationTree'
module System.TaskPipeline.Tasks.LayeredAccess
  ( loadData
  , writeData
  , getLocsMappedTo
  , unsafeRunIOTask
  ) where

import           Prelude                            hiding (id, (.))

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Locations
import           Data.Locations.SerializationMethod
import qualified Data.Map                           as Map
import           Katip
import           System.TaskPipeline.ATask
import           System.TaskPipeline.Resource


-- | Reads some data from all the locations bound to a 'VirtualPath', and merges
-- them thanks to a Monoid instance. Handles the deserialization of the data
-- provided @a@ has some 'DeserializationMethod's available.
--
-- TODO: the list of possible 'DeserializationMethod' should be known
-- statically. We should allow for checking the validity of the deserialization
-- method found in the config before every task is ran.
loadData
  :: (LocationMonad m, KatipContext m, Monoid a)
  => VirtualFile ignored a -- ^ A 'DataSource'
  -> ATask m PipelineResource () a  -- ^ The resulting task
loadData vfile =
  layeredAccessTask' path fname' run
  where
    (path, fname) = vpDeserialToLTPIs vfile
    fname' = fmap (PRscVirtualFile . WithDefaultUsage (vfileUsedByDefault vfile)) fname
    deserials = indexPureDeserialsByFileType $ vfileSerials vfile
    run ft = do
      deserial <- Map.lookup ft deserials
      return $ \_ loc -> do
        r <- case deserial of
          SomeDeserial s f -> f <$> loadFromLoc s loc
        logFM InfoS $ logStr $ "Successfully loaded file '" ++ show loc ++ "'"
        return r

-- | Writes some data to all the locations bound to a 'VirtualPath'
writeData
  :: (LocationMonad m, KatipContext m)
  => VirtualFile a ignored  -- ^ A 'DataSink'
  -> ATask m PipelineResource a ()
writeData vfile =
  layeredAccessTask' path fname' run
  where
    (path, fname) = vpSerialToLTPIs vfile
    fname' = fmap (PRscVirtualFile . WithDefaultUsage (vfileUsedByDefault vfile)) fname
    serials = indexPureSerialsByFileType $ vfileSerials vfile
    run ft = do
      serial <- Map.lookup ft serials
      return $ \input loc -> do
        case serial of
          SomeSerial s f -> persistAtLoc s (f input) loc
        logFM InfoS $ logStr $ "Successfully wrote file '" ++ show loc ++ "'"

-- | Returns the locs mapped to some path in the location tree. It *doesn't*
-- expose this path as a requirement (hence the result list may be empty, as no
-- mapping might exist). SHOULD NOT BE USED UNLESS loadDataTask/writeDataTask
-- cannot do what you want.
getLocsMappedTo :: (Monad m) => [LocationTreePathItem] -> ATask m PipelineResource () [Loc]
getLocsMappedTo path = ATask mempty (\(_,tree) -> return (getLocs tree, tree))
  where
    getLocs tree =
      toListOf (atSubfolderRec path . locTreeNodeTag . rscAccessed . pRscVirtualFile . locLayers . _1) tree

-- | Runs an IO action. IT MUST NOT BE PERFORMING READS OR WRITES.
unsafeRunIOTask
  :: (LocationMonad m)
  => (i -> IO o)
  -> ATask m PipelineResource i o
unsafeRunIOTask f = unsafeLiftToATask (liftIO . f)

-- | A slightly lower-level version of 'layeredAccessTask'. The file to access
-- should be tagged with a PipelineResource, not with a SerialMethod. That
-- permits for instance that the file is by default bound to @PRscVirtualFile
-- Nothing@ (which will correspond to `null` in the yaml config file)
layeredAccessTask'
  :: (LocationMonad m, KatipContext m, Monoid o)
  => [LocationTreePathItem]   -- ^ Folder path
  -> LTPIAndSubtree UnboundPipelineResource  -- ^ File in folder
  -> (SerialMethod -> Maybe (i -> Loc -> m o))
      -- ^ If the 'SerialMethod' is accepted, this function should return @Just
      -- f@, where @f@ is a function taking the input @i@ of the task, the 'Loc'
      -- where it should read/write, and that should perform the access and
      -- return a result @o@. This function will be called once per layer, and
      -- the results of each called will be combined since @o@ must be a
      -- 'Monoid'.
  -> ATask m PipelineResource i o
layeredAccessTask' path fname f =
  liftToATask path (Identity fname) $
    \i (Identity layers) ->
      case layers of
        PRscVirtualFile l -> mconcat <$>
          mapM (access i) (l^..locLayers)
        PRscNothing -> return mempty
        _ -> throwWithPrefix $
          "Unsupported pipeline resource to load.\
          \ Only file paths or 'null' can be used"
  where
    access input (loc, ser) =
      case f ser of
        Nothing -> throwWithPrefix $
          "When accessing " ++ show loc' ++ ", " ++
          show ser ++ " serialization method not supported."
        Just f' -> f' input loc'
      where loc' = addExtToLocIfMissing loc ser
