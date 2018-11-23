{-# LANGUAGE TupleSections     #-}

module System.TaskPipeline.Repetition
  ( module System.TaskPipeline.Repetition.Streaming
  , RepetitionInfo, withRepKey
  , parMapTask
  , parMapTask_
  ) where

import Control.Arrow.Free (mapA)
import Control.Lens
import Katip
import System.TaskPipeline.PTask
import System.TaskPipeline.Repetition.Internal
import System.TaskPipeline.Repetition.Streaming
import Prelude hiding (id, (.))


-- | Makes a 'PTask' repeatable and maps it in parallel over a list
parMapTask
  :: (Show idx, Monad m)
  => RepetitionInfo
  -> PTask m a b
  -> PTask m [(idx,a)] [(idx,b)]
parMapTask ri =
  over ptaskRunnable mapA . makeRepeatable ri

-- | Simply repeats a task which takes no input over a list of indices, and
-- ignores the end result. See 'RepetitionInfo' for how these indices are
-- used. See 'parMapTask' for a more complete version.
parMapTask_
  :: (Show idx, Monad m)
  => RepetitionInfo
  -> PTask m () b
  -> PTask m [idx] ()
parMapTask_ ri task =
   arr (map (, ())) >>> parMapTask ri task >>> arr (const ())
