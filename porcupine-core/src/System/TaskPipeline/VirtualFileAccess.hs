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
module System.TaskPipeline.VirtualFileAccess
  ( -- * Reexports
    Typeable

    -- * High-level API
  , loadData
  , loadDataStream
  , loadLast
  , writeData
  , writeDataStream

    -- * Lower-level API
  , accessVirtualFile
  , getAccessFunctions
  , getLocsMappedTo
  , streamHeadTask
  ) where

import           Prelude                            hiding (id, (.))

import           Control.Lens
import           Control.Monad                      (forM)
import qualified Data.Foldable                      as F
import qualified Data.HashMap.Strict                as HM
import           Data.Locations
import           Data.Maybe                         (maybe)
import           Data.Monoid
import           Data.Typeable
import           Streaming                          (Of (..), Stream)
import qualified Streaming.Prelude                  as S
import           System.TaskPipeline.PTask
import           System.TaskPipeline.PTask.Internal
import           System.TaskPipeline.ResourceTree


streamHeadTask :: (KatipContext m) => PTask m (Stream (Of (i, a)) m r) a
streamHeadTask = unsafeLiftToPTask $ \s ->
  maybe (error $ "streamHeadTask: No value in the output stream") snd <$> S.head_ s

-- | Uses only the read part of a 'VirtualFile'. It is therefore considered as a
-- pure 'DataSource'. For practical reasons the task input is () rather than
-- Void.
loadData
  :: (LocationMonad m, KatipContext m, Monoid a, Typeable a)
  => VirtualFile ignored a -- ^ A 'DataSource'
  -> PTask m () a  -- ^ The resulting task
loadData vf = arr (\_ -> S.yield ([] :: [Int]))
          >>> loadDataStream [] vf
          >>> streamHeadTask

-- | Loads a stream of repeated occurences of a VirtualFile, from a stream of
-- indices. The process is lazy: the data will actually be read when the
-- resulting stream is consumed.
loadDataStream :: (Show i, LocationMonad m, KatipContext m, Monoid a, Typeable a)
               => [LocVariable]
               -> VirtualFile ignored a -- ^ A 'DataSource'
               -> PTask m (Stream (Of [i]) m r) (Stream (Of ([i], a)) m r)
loadDataStream repIndices vf =
      arr (S.map (, error "loadDataStream: THIS IS VOID"))
  >>> accessVirtualFile repIndices (makeSource vf)

-- | Like 'loadData', but doesn't require your data to be a monoid. Will always
-- load the last layer bound to the 'VirtualFile'.
loadLast
  :: (LocationMonad m, KatipContext m, Typeable a)
  => VirtualFile ignored a -- ^ A 'DataSource'
  -> PTask m () a  -- ^ The resulting task
loadLast vf = loadData (rmap (Last . Just) vf') >>> arr get
  where
    vf' = vf & vfileUsage .~ MustBeMapped
    get (Last (Just x)) = x
    get _               = error "loadLast: THIS SHOULD NOT HAPPEN" -- Won't be evaluated

-- | Uses only the write part of a 'VirtualFile'. It is therefore considered as
-- a pure 'DataSink'.
writeData
  :: (LocationMonad m, KatipContext m, Typeable a)
  => VirtualFile a ignored  -- ^ A 'DataSink'
  -> PTask m a ()
writeData vf = arr (\a -> S.yield ([] :: [Int], a))
           >>> writeDataStream [] vf

writeDataStream :: (Show i, LocationMonad m, KatipContext m, Typeable a)
                => [LocVariable]
                -> VirtualFile a ignored -- ^ A 'DataSink'
                -> PTask m (Stream (Of ([i], a)) m r) r
writeDataStream repIndices vf =
  accessVirtualFile repIndices (makeSink vf) >>> unsafeLiftToPTask S.effects

-- | When building the pipeline, stores into the location tree the way to read
-- or write the required resource. When running the pipeline, access the
-- instances of this ressource corresponding to the values of some repetition
-- indices.
accessVirtualFile
  :: forall m a b i r.
     (LocationMonad m, KatipContext m, Typeable a, Typeable b, Monoid b, Show i)
  => [LocVariable]
  -> VirtualFile a b
  -> PTask m (Stream (Of ([i], a)) m r) (Stream (Of ([i], b)) m r)
accessVirtualFile repIndices vfile =
  getAccessFunctions path (Identity fname) $
    \inputStream (Identity mbAction) -> case mbAction of
      DataAccessNode _ (action :: LocVariableMap -> a' -> m b') ->
        case (eqT :: Maybe (a :~: a'), eqT :: Maybe (b :~: b')) of
          (Just Refl, Just Refl)
            -> return $ S.mapM (runOnce action) inputStream
          _ -> err vfile' "input or output types don't match"
      _ -> err vfile' "no access action available"
  where
    runOnce :: (LocVariableMap -> a -> m b) -> ([i], a) -> m ([i], b)
    runOnce action (ixVals, input) = (ixVals,) <$> action lvMap input
      where lvMap = HM.fromList $ zip repIndices $ map show ixVals
    vfile' = case repIndices of
      [] -> vfile
      _  -> vfile & over (vfileSerials.serialsRepetitionKeys) (repIndices++)
    path = init $ vfile' ^. vfilePath
    fname = file (last $ vfile' ^. vfilePath) $ VirtualFileNode vfile'

err :: (KatipContext m, MonadThrow m) => VirtualFile a1 b -> String -> m a2
err vfile s = throwWithPrefix $ "accessVirtualFile (" ++ showVFilePath vfile ++ "): " ++ s

-- | Wraps in a task a function that needs to access some items present in a
-- subfolder of the 'LocationTree' and mark these accesses as done.
getAccessFunctions
  :: (MonadThrow m, KatipContext m, Traversable t)
  => [LocationTreePathItem]  -- ^ Path to subfolder in 'LocationTree'
  -> t (LTPIAndSubtree VirtualFileNode)    -- ^ Items of interest in the subfolder
  -> (i -> t (DataAccessNode m) -> m o)       -- ^ What to run with these items
  -> PTask m i o           -- ^ The resulting PTask
getAccessFunctions path filesToAccess writeFn = makePTask tree runAccess
  where
    tree = foldr (\pathItem subtree -> folderNode [ pathItem :/ subtree ])
                 (folderNode $ F.toList filesToAccess) path
    runAccess rscTree input = do
      let mbSubtree = rscTree ^? atSubfolderRec path
      subtree <- case mbSubtree of
        Just s -> return s
        Nothing -> throwWithPrefix $
          "path '" ++ show path ++ "' not found in the LocationTree"
      nodeTags <- forM filesToAccess $ \(filePathItem :/ _) -> do
        case subtree ^? atSubfolder filePathItem . locTreeNodeTag of
          Nothing -> throwWithPrefix $
            "path '" ++ show filePathItem ++ "' not found in the LocationTree"
          Just tag -> return tag
      writeFn input nodeTags

-- | Returns the locs mapped to some path in the location tree. It *doesn't*
-- expose this path as a requirement (hence the result list may be empty, as no
-- mapping might exist). SHOULD NOT BE USED UNLESS loadData/writeData cannot do
-- what you want.
getLocsMappedTo :: (KatipContext m)
                => [LocationTreePathItem] -> PTask m () [LocWithVars]
getLocsMappedTo path = runnableWithoutReqs $ withRunnableState $
                         \state _ -> return $ getLocs $ state^.ptrsDataAccessTree
  where
    getLocs tree =
      case tree ^? (atSubfolderRec path . locTreeNodeTag) of
        Just (MbDataAccessNode locs _) -> locs
        _                              -> []
