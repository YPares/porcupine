{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module implements a Foldl-based interface for arrow computations
-- compatible with the <https://hackage.haskell.org/package/foldl foldl
-- library>. Use 'generalizeA' and 'generalizeM' to convert folds to
-- 'FoldA'. This is the most general way to repeat a 'PTask' over some an input
-- (list, array, stream, etc.).
--
-- This API is still experimental and might be subject to changes in the future

module System.TaskPipeline.Repetition.Foldl
  ( module Control.Arrow.FoldA
  , RepInfo(..)
  , TRIndex(..)
  , HasTRIndex(..)
  , generalizeM
  , generalizeM_
  , foldlTask
  , foldStreamTask
  , runFoldAOverPTask
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

instance (HasTRIndex a)
      => HasTRIndex (Pair x a) where
  getTRIndex (Pair _ a) = getTRIndex a

-- | Runs a 'FoldA' created with 'arrowFold', 'generalizeA', 'unsafeGeneralizeM',
-- or a composition of such folds.
--
-- You shouldn't use 'runFoldAOverPTask' directly in client code, rather you should
-- specialize it to some collection. See e.g 'foldStreamTask' or 'foldlTask'.
runFoldAOverPTask
  :: (HasTRIndex a, KatipContext m)
  => (forall ar x. (ArrowChoice ar)
       => (forall inp out. (inp -> m out) -> ar inp out)
       -> ar (Pair x a) x
       -> ar (x, col) (x, r))
       -- ^ This function receives a function to wrap an action in the @m@ monad
       -- and the step to repeat. It should consume the collection
  -> RepInfo  -- ^ How to log the repeated task
  -> FoldA (PTask m) i a b  -- ^ The 'FoldA' to run
  -> PTask m (i, col) (b, r)
runFoldAOverPTask loopStep ri (FoldA step_ start done) =
  (reqs, runnable) ^. from splitTask
  where
    (reqsStep, runnableStep)   = makeTaskRepeatable ri step_ ^. splitTask
    (reqsStart, runnableStart) = start ^. splitTask
    (reqsDone, runnableDone)   = done ^. splitTask
    reqs = reqsStart <> reqsStep <> reqsDone
    runnable =
      first runnableStart >>> loopStep (withRunnableState . const) runnableStep
                          >>> first runnableDone

-- | Consumes a Stream with a 'FoldA' created with 'arrowFold', 'generalizeA',
-- 'unsafeGeneralizeM', or a composition of such folds.
foldStreamTask
  :: (HasTRIndex a, KatipContext m)
  => RepInfo  -- ^ How to log the repeated task
  -> FoldA (PTask m) i a b  -- ^ The FoldA to run
  -> PTask m (i, Stream (Of a) m r) (b, r)
foldStreamTask = runFoldAOverPTask $ \wrap step ->
  let
    consumeStream = proc (acc, stream) -> do
      firstElem <- wrap S.next -< stream
      case firstElem of
        Left r -> returnA -< (acc, r)
        Right (a, stream') -> do
          !acc' <- step -< Pair acc a
          consumeStream -< (acc', stream')
  in consumeStream

-- | Consumes a Foldable with a 'FoldA' over 'PTask'.
--
-- See 'arrowFold' to create such a 'FoldA'
foldlTask
  :: (Foldable f, HasTRIndex a, KatipContext m)
  => RepInfo  -- ^ How to log the repeated task
  -> FoldA (PTask m) i a b
  -> PTask m (i, f a) b
foldlTask ri fld =
  arr (second S.each) >>> foldStreamTask ri fld >>> arr fst

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
