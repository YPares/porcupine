module System.TaskPipeline.Repetition
  ( module System.TaskPipeline.Repetition.Streaming
  , parMapTask
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
  => LocVariable
  -> Maybe Verbosity
  -> PTask m a b
  -> PTask m [(idx,a)] [(idx,b)]
parMapTask rk mv =
  over ptaskRunnable mapA . makeRepeatable rk mv
