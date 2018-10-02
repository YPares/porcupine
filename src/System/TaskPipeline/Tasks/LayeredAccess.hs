{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
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
  , accessVirtualFile
  , getLocsMappedTo
  , unsafeRunIOTask
  ) where

import           Prelude                            hiding (id, (.))

import           Control.Lens
import           Control.Monad.IO.Class
import qualified Data.HashMap.Strict                as HM
import           Data.Locations
import           Data.Locations.SerializationMethod
import           Data.Monoid                        (First (..))
import           Data.Typeable
import           Katip
import           System.TaskPipeline.ATask
import           System.TaskPipeline.Resource


-- | Uses only the read part of a 'VirtualFile'. It is therefore considered as a
-- pure 'DataSource'. For practical reasons the task input is () rather than
-- Void.
--
-- See 'accessVirtualFile'.
loadData
  :: (LocationMonad m, KatipContext m, Monoid a)
  => VirtualFile ignored a -- ^ A 'DataSource'
  -> ATask m PipelineResource () a  -- ^ The resulting task
loadData vf = arr (const $ error "THIS IS VOID")  -- Won't be evaluated
          >>> (accessVirtualFile $ makeSource vf)

-- | Uses only the write part of a 'VirtualFile'. It is therefore considered as
-- a pure 'DataSink'.
--
-- See 'accessVirtualFile'
writeData
  :: (LocationMonad m, KatipContext m)
  => VirtualFile a ignored  -- ^ A 'DataSink'
  -> ATask m PipelineResource a ()
writeData = accessVirtualFile . makeSink

virtualFileToPipelineResource :: VirtualFile a b -> ([LocationTreePathItem], LTPIAndSubtree UnboundPipelineResource)
virtualFileToPipelineResource vf
  | null (vfilePath vf) = error "virtualFileToPipelineResource: EMPTY PATH"
  | otherwise           = (init p, fname)
  where
    p = vfilePath vf
    s = vfileSerials vf
    First mbopts = (,,) <$> vfileBidirProof vf
                        <*> serialWriterToConfig (serialWriters s)
                        <*> (readFromConfigDefault <$> serialReaderFromConfig (serialReaders s))
    extension = case serialDefaultExt s of
      First (Just ext) -> associatedFileType ext
      First Nothing    -> LocDefault
    fname = file (last p) $ case mbopts of
      Just (Refl, WriteToConfigFn convert, defVal) -> PRscOptions $ RecOfOptions $ convert defVal
      Nothing -> PRscVirtualFile $ WithDefaultUsage (vfileUsedByDefault vf) extension

-- | Writes some data to all the locations bound to a 'VirtualFile' if this
-- 'VirtualFile' has writers, then reads some data over several layers from it
-- (and merges them thanks to a Monoid instance) if this 'VirtualFile' has
-- readers.
--
-- TODO: the list of possible 'DeserializationMethod' should be known
-- statically. We should allow for checking the validity of the deserialization
-- method found in the config before every task is ran.
accessVirtualFile
  :: (LocationMonad m, KatipContext m, Monoid b)
  => VirtualFile a b
  -> ATask m PipelineResource a b
accessVirtualFile vfile =
  liftToATask path (Identity fname) $
    \input (Identity layers) ->
      case layers of
        PRscNothing -> return mempty
        PRscVirtualFile l -> mconcat <$>
          mapM (access input) (l^..locLayers)
        PRscOptions (RecOfOptions newDocRec) ->
          case serialReaderFromConfig $ serialReaders $ vfileSerials vfile of
            First Nothing -> err vfile "this path doesn't accept options"
            First (Just (ReadFromConfig _ convert)) ->
              case cast newDocRec of
                Nothing -> err vfile "the DocRec received isn't of the expected type"
                Just newDocRec' -> return $ convert newDocRec'
        _ -> err vfile "unsupported pipeline resource to load. SHOULD NOT HAPPEN."
  where
    (path, fname) = virtualFileToPipelineResource vfile
    writers = indexPureSerialsByFileType $ vfileSerials vfile
    readers = indexPureDeserialsByFileType $ vfileSerials vfile
    access input (locWithoutExt, ser) = do
      let loc = addExtToLocIfMissing locWithoutExt ser
      case HM.lookup ser writers of
        Nothing -> return ()
        Just (WriteToLocFn writer) -> do
          writer input loc
          logFM InfoS $ logStr $ "Successfully wrote file '" ++ show loc ++ "'"
      case HM.lookup ser readers of
        Nothing -> return mempty
        Just (ReadFromLocFn reader) -> do
          r <- reader loc
          logFM InfoS $ logStr $ "Successfully loaded file '" ++ show loc ++ "'"
          return r

-- | When building the pipeline, stores into the location tree the way to read
-- or write the required resource. When running the pipeline, it is handed the
-- function to actually access the data.
accessVirtualFile'
  :: (LocationMonad m, KatipContext m, Typeable a, Typeable b, Monoid b)
  => VirtualFile a b
  -> ATask m (VirtualFileOrData m) a b
accessVirtualFile' vfile =
  liftToATask path (Identity fname) $
    \input (Identity (AccessDataNode' action')) -> do
      AccessDataNode action <- case action' of
            First (Just a) -> return a
            First Nothing -> err vfile "no action available for access"
      res <- case cast input of
        Just input' -> action input'
        Nothing -> err vfile "input types don't match"
      case cast res of
        Just res' -> return res'
        Nothing -> err vfile "output types don't match"
  where
    path = init $ vfilePath vfile
    fname = file (last $ vfilePath vfile) $ VirtualFileNode' $ Just $ VirtualFileNode $ removeVFilePath vfile

err :: (KatipContext m, MonadThrow m) => VirtualFile a1 b -> [Char] -> m a2
err vfile s = throwWithPrefix $ "accessVirtualFile: " ++ show (vfilePath vfile) ++ ": " ++ s

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
