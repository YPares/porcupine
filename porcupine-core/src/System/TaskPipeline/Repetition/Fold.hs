{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TupleSections             #-}

module System.TaskPipeline.Repetition.Fold
  ( module Control.Arrow.FoldA
  , RepInfo(..)
  , HasTaskRepetitionIndex(..)
  , ptaskFold
  , unsafeGeneralizeM
  , unsafeGeneralizeFnM
  , foldlTask
  , foldStreamTask
  , premapMaybe
  ) where

import           Control.Arrow.FoldA
import           Control.Lens                            hiding (Fold)
import           Data.Locations
import           Prelude                                 hiding (id, (.))
import           Streaming                               (Of (..), Stream)
import qualified Streaming.Prelude                       as S
import           System.TaskPipeline.PTask
import           System.TaskPipeline.PTask.Internal
import           System.TaskPipeline.Repetition.Internal


-- * Folding data with a PTask

-- | Turns a 'FoldM' in some monad to a 'FoldA' compatible with 'foldTask'
unsafeGeneralizeM :: (KatipContext m)
                  => FoldM m a b -> FoldA (PTask m) i a b
unsafeGeneralizeM (FoldM step start done) =
  FoldA (unsafeLiftToPTask $ \(Pair a x) -> step a x)
        (unsafeLiftToPTask $ const start)
        (unsafeLiftToPTask done)

data RunningFoldM m a b =
  forall x. RFM (x -> a -> m x) !x (x -> m b)

-- | Turns a function creating a 'FoldM' into a 'FoldA' over 'PTasks'
unsafeGeneralizeFnM :: (KatipContext m)
                    => (i -> FoldM m a b)
                    -> FoldA (PTask m) i a b
unsafeGeneralizeFnM f =
  FoldA (unsafeLiftToPTask $ \(Pair (RFM step acc done) x) -> do
            acc' <- step acc x
            return $ RFM step acc' done)
        (unsafeLiftToPTask $ \i ->
            case f i of
              FoldM step start done -> do
                initAcc <- start
                return $ RFM step initAcc done)
        (unsafeLiftToPTask $ \(RFM _ acc done) -> done acc)

-- | Creates a 'FoldA' from a 'PTask'.
ptaskFold :: PTask m (acc,input) acc -- ^ The folding task
          -> FoldA' (PTask m) input acc
ptaskFold step =
  FoldA (arr onInput >>> step) id id
  where
    onInput (Pair acc x) = (acc,x)

newtype PairWithRepeatable x a = PWR { unPWR :: Pair x a }

instance (HasTaskRepetitionIndex a)
      => HasTaskRepetitionIndex (PairWithRepeatable x a) where
  getTaskRepetitionIndex (PWR (Pair _ a)) = getTaskRepetitionIndex a

-- | Just before running the fold, we have to make the step part repeatable
makeStepRepeatable :: (HasTaskRepetitionIndex a, Monad m)
                   => RepInfo
                   -> PTask m (Pair acc a) b
                   -> PTask m (Pair acc a) b
makeStepRepeatable ri step =
  arr PWR >>> makeRepeatable ri (arr unPWR >>> step)

-- | Consumes a Stream with a 'FoldA' created with 'ptaskFold', 'generalizeA',
-- 'unsafeGeneralizeM', or a composition of such folds.
foldStreamTask
  :: (HasTaskRepetitionIndex a, KatipContext m)
  => RepInfo  -- ^ How to log the repeated task
  -> FoldA (PTask m) i a b
  -> PTask m (i, Stream (Of a) m r) (b, r)
foldStreamTask ri (FoldA step_ start done) =
  (reqs, runnable) ^. from splittedPTask
  where
    (reqsStep, runnableStep)   = makeStepRepeatable ri step_ ^. splittedPTask
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
  :: (HasTaskRepetitionIndex a, KatipContext m)
  => RepInfo  -- ^ How to log the repeated task
  -> FoldA (PTask m) i a b
  -> PTask m (i,[a]) b
foldlTask ri fld = arr (\(i,l) -> (i,S.each l))
            >>> foldStreamTask ri fld >>> arr fst

-- | Allows to filter out some data before it is taken into account by the FoldA
-- of PTask
premapMaybe :: (a -> Maybe a')
            -> FoldA (PTask m) i a' b
            -> FoldA (PTask m) i a b
premapMaybe f (FoldA step start done) = FoldA step' start done
  where
    step' = step & over ptaskRunnablePart
      (\run -> proc (Pair acc input) -> do
          case f input of
            Nothing     -> returnA -< acc
            Just input' -> run -< Pair acc input')
