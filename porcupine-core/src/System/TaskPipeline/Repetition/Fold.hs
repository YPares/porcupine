{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}


module System.TaskPipeline.Repetition.Fold
  ( module Control.Arrow.FoldA
  , RepInfo(..)
  , TRIndex(..)
  , HasTRIndex(..)
  , generalizeM
  , generalizeM_
  , foldlTask
  , foldStreamTask
  , premapMaybe
  ) where

import           Control.Arrow.FoldA
import           Control.Lens                            hiding (Fold)
import           Data.Locations
import           Prelude                                 hiding (id)
import           Streaming                               (Of (..), Stream)
import qualified Streaming.Prelude                       as S
import           System.TaskPipeline.PTask
import           System.TaskPipeline.PTask.Internal
import           System.TaskPipeline.Repetition.Internal


-- * Folding data with a PTask

data RunningFoldM m a b =
  forall x. RFM (x -> a -> m x) !x (x -> m b)

-- | Turns a function creating a 'FoldM' into a 'FoldA' over 'PTasks'
generalizeM :: (KatipContext m)
            => (i -> FoldM m a b)
            -> FoldA (PTask m) i a b
generalizeM f =
  FoldA (toTask $ \(Pair (RFM step acc done) x) -> do
            acc' <- step acc x
            return $ RFM step acc' done)
        (toTask $ \i ->
            case f i of
              FoldM step start done -> do
                initAcc <- start
                return $ RFM step initAcc done)
        (toTask $ \(RFM _ acc done) -> done acc)

-- | Turns a 'FoldM' in some monad to a 'FoldA' compatible with 'foldTask'
--
-- This is a version of 'generalizeM' for when your initial accumulator doesn't
-- need to be computed by a PTask
generalizeM_ :: (KatipContext m)
             => FoldM m a b -> FoldA (PTask m) i a b
generalizeM_ (FoldM step start done) =
  FoldA (toTask $ \(Pair a x) -> step a x)
        (toTask $ const start)
        (toTask done)

newtype PairWithRepeatable x a = PWR { unPWR :: Pair x a }

instance (HasTRIndex a)
      => HasTRIndex (PairWithRepeatable x a) where
  getTRIndex (PWR (Pair _ a)) = getTRIndex a

-- | Just before running the fold, we have to make the step part repeatable
makeStepRepeatable :: (HasTRIndex a, KatipContext m)
                   => RepInfo
                   -> PTask m (Pair acc a) b
                   -> PTask m (Pair acc a) b
makeStepRepeatable ri step =
  arr PWR >>> makeTaskRepeatable ri (arr unPWR >>> step)

-- | Consumes a Stream with a 'FoldA' created with 'taskFold', 'generalizeA',
-- 'unsafeGeneralizeM', or a composition of such folds.
foldStreamTask
  :: (HasTRIndex a, KatipContext m)
  => RepInfo  -- ^ How to log the repeated task
  -> FoldA (PTask m) i a b
  -> PTask m (i, Stream (Of a) m r) (b, r)
foldStreamTask ri (FoldA step_ start done) =
  (reqs, runnable) ^. from splitTask
  where
    (reqsStep, runnableStep)   = makeStepRepeatable ri step_ ^. splitTask
    (reqsStart, runnableStart) = start ^. splitTask
    (reqsDone, runnableDone)   = done ^. splitTask
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
  :: (HasTRIndex a, KatipContext m)
  => RepInfo  -- ^ How to log the repeated task
  -> FoldA (PTask m) i a b
  -> PTask m (i,[a]) b
foldlTask ri fld = arr (second S.each)
               >>> foldStreamTask ri fld >>> arr fst

-- | Allows to filter out some data before it is taken into account by the FoldA
-- of PTask
--
-- We provide a implementation specific to PTask because a general
-- implementation requires ArrowChoice
premapMaybe :: (a -> Maybe a')
            -> FoldA (PTask m) i a' b
            -> FoldA (PTask m) i a b
premapMaybe f (FoldA step start done) = FoldA step' start done
  where
    step' = step & over taskRunnablePart
      (\run -> proc (Pair acc input) ->
          case f input of
            Nothing     -> returnA -< acc
            Just input' -> run -< Pair acc input')
