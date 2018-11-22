{-# LANGUAGE Arrows            #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module System.TaskPipeline.Repetition.Fold where

import           Control.Arrow.FoldA
import           Control.Lens
import           Data.Locations
import           Prelude                               hiding (id, (.))
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
          => LocVariable
          -> Maybe Verbosity
          -> PTask m (x,acc) acc
          -> acc
          -> FoldA (PTask m) (idx,x) acc
ptaskFold rk mv step init =
  FoldA (arr onInput >>> makeRepeatable rk mv step >>> arr snd)
        (pure init) id
  where
    onInput (Pair acc (idx,x)) = (idx,(x,acc))

-- | Runs a 'FoldA' created with 'ptaskFold', 'generalizeA',
-- 'unsafeGeneralizeM', or a composition of such folds.
--
-- IMPORTANT : If you created the 'FoldA' yourself, this is safe only if the
-- step PTask in the fold has been made repeatable, else files might be
-- overwritten. See 'makeRepeatable'.
foldTask
  :: FoldA (PTask m) a b
  -> PTask m [a] b
foldTask (FoldA step start done) =
  (reqs, runnable) ^. from splittedPTask
  where
    (reqsStep, runnableStep)   = step ^. splittedPTask
    (reqsStart, runnableStart) = start ^. splittedPTask
    (reqsDone, runnableDone)   = done ^. splittedPTask
    reqs = reqsStart <> reqsStep <> reqsDone
    runnable =
      id &&&& (pure () >>> runnableStart) >>> loopStep >>> runnableDone
    loopStep = proc (Pair list acc) -> do
      case list of
        []     -> returnA -< acc
        (a:as) -> do
          acc' <- runnableStep -< Pair acc a
          loopStep -< Pair as acc'
