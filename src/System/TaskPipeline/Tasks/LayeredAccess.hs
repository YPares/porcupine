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
  ( layeredAccessTask
  , layeredAccessTask'
  , loadDataTask
  , writeDataTask
  ) where

import           Prelude                            hiding (id, (.))

import           Control.Lens
import           Data.Locations
import           Data.Locations.SerializationMethod
import qualified Data.Map                           as Map
import qualified Katip                              as K
import           System.TaskPipeline.ATask
import           System.TaskPipeline.Resource


-- | Reads some data from all the locations bound to a 'VirtualPath', and merges
-- them thanks to a Monoid instance. Handles the deserialization of the data
-- provided @a@ has some 'DeserializationMethod's available.
--
-- TODO: the list of possible 'DeserializationMethod' should be known
-- statically. We should allow for checking the validity of the deserialization
-- method found in the config before every task is ran.
loadDataTask
  :: (LocationMonad m, K.KatipContext m, Monoid b)
  => VirtualFile ignored a -- ^ A 'DataSource'
  -> String  -- ^ A name for the task (for the error message if the wanted
             -- 'SerializationMethod' isn't supported)
  -> ATask m PipelineResource (a -> b) b  -- ^ The resulting task takes in input
                                          -- a function to extract a Monoid from
                                          -- the data it reads. Usually it will
                                          -- just be 'id', but that can permit
                                          -- to use runtime data to transform
                                          -- the data that is read into a
                                          -- Monoid.
loadDataTask vfile taskName =
  layeredAccessTask' path fname' taskName run
  where
    (path, fname) = vpDeserialToLTPIs vfile
    fname' = fmap (PRscVirtualFile . WithDefaultUsage (vfileUsedByDefault vfile)) fname
    deserials = indexPureDeserialsByFileType $ vfileSerials vfile
    run ft = do
      deserial <- Map.lookup ft deserials
      return $ \f' loc -> do
        r <- case deserial of
          SomeDeserial s f -> f' . f <$> loadFromLoc s loc
        K.logFM K.InfoS $ K.logStr $ "Successfully loaded file '" ++ show loc ++ "'"
        return r

-- | Writes some data to all the locations bound to a 'VirtualPath'
writeDataTask
  :: (LocationMonad m, K.KatipContext m)
  => VirtualFile a ignored  -- ^ A 'DataSink'
  -> String  -- ^ A name for the task (for the error message if the wanted
             -- 'SerializationMethod' isn't supported)
  -> ATask m PipelineResource a ()
writeDataTask vfile taskName =
  layeredAccessTask' path fname' taskName run
  where
    (path, fname) = vpSerialToLTPIs vfile
    fname' = fmap (PRscVirtualFile . WithDefaultUsage (vfileUsedByDefault vfile)) fname
    serials = indexPureSerialsByFileType $ vfileSerials vfile
    run ft = do
      serial <- Map.lookup ft serials
      return $ \input loc -> do
        case serial of
          SomeSerial s f -> persistAtLoc s (f input) loc
        K.logFM K.InfoS $ K.logStr $ "Successfully wrote file '" ++ show loc ++ "'"

-- | Accesses each layer mapped to the required file and combines the result of
-- each access.
layeredAccessTask
  :: (LocationMonad m, Monoid o)
  => [LocationTreePathItem]   -- ^ Folder path
  -> LTPIAndSubtree SerialMethod  -- ^ File in folder
  -> String  -- ^ A name for the task (for the error message if wanted
             -- SerialMethod isn't supported)
  -> (SerialMethod -> Maybe (i -> Loc -> m o))
      -- ^ If the 'SerialMethod' is accepted, this function should return @Just
      -- f@, where @f@ is a function taking the input @i@ of the task, the 'Loc'
      -- where it should read/write, and that should perform the access and
      -- return a result @o@. This function will be called once per layer, and
      -- the results of each called will be combined since @o@ must be a
      -- 'Monoid'.
  -> ATask m PipelineResource i o
layeredAccessTask path fname taskName f =
  layeredAccessTask' path (PRscSerialMethod <$> fname) taskName f

-- | A slightly lower-level version of 'layeredAccessTask'. The file to access
-- should be tagged with a PipelineResource, not with a SerialMethod. That
-- permits for instance that the file is by default bound to @PRscVirtualFile
-- Nothing@ (which will correspond to `null` in the yaml config file)
layeredAccessTask'
  :: (LocationMonad m, Monoid o)
  => [LocationTreePathItem]   -- ^ Folder path
  -> LTPIAndSubtree UnboundPipelineResource  -- ^ File in folder
  -> String  -- ^ A name for the task (for the error message if wanted
             -- SerialMethod isn't supported)
  -> (SerialMethod -> Maybe (i -> Loc -> m o))
      -- ^ If the 'SerialMethod' is accepted, this function should return @Just
      -- f@, where @f@ is a function taking the input @i@ of the task, the 'Loc'
      -- where it should read/write, and that should perform the access and
      -- return a result @o@. This function will be called once per layer, and
      -- the results of each called will be combined since @o@ must be a
      -- 'Monoid'.
  -> ATask m PipelineResource i o
layeredAccessTask' path fname taskName f =
  liftToATask path (Identity fname) taskName $
    \i (Identity layers) ->
      case layers of
        PRscVirtualFile l -> mconcat <$>
          mapM (access i) (l^..locLayers)
        PRscNothing -> return mempty
        _ -> throwM $ TaskRunError $
          taskName ++ ": Unsupported pipeline resource to load.\
                      \ Only file paths or 'null' can be used"
  where
    access input (loc, ser) =
      case f ser of
        Nothing -> throwM $ TaskRunError $
          taskName ++ ": When accessing " ++ show loc' ++ ", " ++
          show ser ++ " serialization method not supported."
        Just f' -> f' input loc'
      where loc' = addExtToLocIfMissing loc ser
