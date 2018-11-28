{-# LANGUAGE Arrows            #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module System.TaskPipeline.Repetition.Fold
  ( ptaskFold
  , unsafeGeneralizeM
  , foldlTask
  , foldStreamTask
  ) where

import           Control.Arrow.FoldA
import           Control.Lens
import           Data.Locations
import           Prelude                                 hiding (id, (.))
import           Streaming                               (Of (..), Stream)
import qualified Streaming.Prelude                       as S
import           System.TaskPipeline.PTask
import           System.TaskPipeline.PTask.Internal
import           System.TaskPipeline.Repetition.Internal


-- * Folding data with a PTask

-- | Turns a fold in some monad to a fold compatible with 'foldTask'
unsafeGeneralizeM :: (KatipContext m)
                  => FoldM m a b -> FoldA (PTask m) a b
unsafeGeneralizeM (FoldM step start done) =
  FoldA (unsafeLiftToPTask $ \(Pair a x) -> step a x)
        (unsafeLiftToPTask $ const start)
        (unsafeLiftToPTask done)

-- | Creates a 'FoldA' from a 'PTask'.
ptaskFold :: (Show idx, Monad m)
          => RepInfo
          -> PTask m (x,acc) acc
          -> acc
          -> FoldA (PTask m) (idx,x) acc
ptaskFold ri step initAcc =
  FoldA (arr onInput >>> makeRepeatable ri step >>> arr snd)
        (pure initAcc) id
  where
    onInput (Pair acc (idx,x)) = (idx,(x,acc))

-- | Consumes a Stream with a 'FoldA' created with 'ptaskFold', 'generalizeA',
-- 'unsafeGeneralizeM', or a composition of such folds.
--
-- IMPORTANT : If you created the 'FoldA' yourself, this is safe only if the
-- `step` PTask in the fold has been made repeatable, else files might be
-- overwritten. See 'makeRepeatable'.
foldStreamTask
  :: (KatipContext m)
  => FoldA (PTask m) a b
  -> PTask m (Stream (Of a) m r) (b, r)
foldStreamTask (FoldA step start done) =
  (reqs, runnable) ^. from splittedPTask
  where
    (reqsStep, runnableStep)   = step ^. splittedPTask
    (reqsStart, runnableStart) = start ^. splittedPTask
    (reqsDone, runnableDone)   = done ^. splittedPTask
    reqs = reqsStart <> reqsStep <> reqsDone
    runnable =
      id &&&& (pure () >>> runnableStart) >>> loopStep >>> first runnableDone
    loopStep = proc (Pair stream acc) -> do
      firstElem <- withRunnableState (const S.next) -< stream
      case firstElem of
        Left r -> returnA -< (acc, r)
        Right (a, stream') -> do
          acc' <- runnableStep -< Pair acc a
          loopStep -< Pair stream' acc'

-- | Consumes a list with a 'FoldA' over 'PTask'
foldlTask
  :: (KatipContext m)
  => FoldA (PTask m) a b
  -> PTask m [a] b
foldlTask fld = arr S.each >>> foldStreamTask fld >>> arr fst
