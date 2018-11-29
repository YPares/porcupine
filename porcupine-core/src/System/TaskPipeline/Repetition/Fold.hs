{-# LANGUAGE Arrows            #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module System.TaskPipeline.Repetition.Fold
  ( module Control.Arrow.FoldA
  , RepInfo(..)
  , ptaskFold
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
                  => FoldM m a b -> FoldA (PTask m) i a b
unsafeGeneralizeM (FoldM step start done) =
  FoldA (unsafeLiftToPTask $ \(Pair a x) -> step a x)
        (unsafeLiftToPTask $ const start)
        (unsafeLiftToPTask done)

-- | Creates a 'FoldA' from a 'PTask'.
ptaskFold :: (Show idx, Monad m)
          => RepInfo  -- ^ How to log the repeated task
          -> PTask m (acc,input) acc -- ^ The folding task
          -> FoldA' (PTask m) (idx,input) acc
ptaskFold ri step =
  FoldA (arr onInput >>> makeRepeatable ri step >>> arr snd) id id
  where
    onInput (Pair acc (idx,x)) = (idx,(acc,x))

-- | Consumes a Stream with a 'FoldA' created with 'ptaskFold', 'generalizeA',
-- 'unsafeGeneralizeM', or a composition of such folds.
--
-- IMPORTANT : If you created the 'FoldA' yourself, this is safe only if the
-- `step` PTask in the fold has been made repeatable, else files might be
-- overwritten. See 'makeRepeatable'.
foldStreamTask
  :: (KatipContext m)
  => FoldA (PTask m) i a b
  -> PTask m (i, Stream (Of a) m r) (b, r)
foldStreamTask (FoldA step start done) =
  (reqs, runnable) ^. from splittedPTask
  where
    (reqsStep, runnableStep)   = step ^. splittedPTask
    (reqsStart, runnableStart) = start ^. splittedPTask
    (reqsDone, runnableDone)   = done ^. splittedPTask
    reqs = reqsStart <> reqsStep <> reqsDone
    runnable =
      first runnableStart >>> loopStep >>> first runnableDone
    loopStep = proc (acc, stream) -> do
      firstElem <- withRunnableState (const S.next) -< stream
      case firstElem of
        Left r -> returnA -< (acc, r)
        Right (a, stream') -> do
          acc' <- runnableStep -< Pair acc a
          loopStep -< (acc', stream')

-- | Consumes a list with a 'FoldA' over 'PTask'
foldlTask
  :: (KatipContext m)
  => FoldA (PTask m) i a b
  -> PTask m (i,[a]) b
foldlTask fld = arr (\(i,l) -> (i,S.each l))
            >>> foldStreamTask fld >>> arr fst
