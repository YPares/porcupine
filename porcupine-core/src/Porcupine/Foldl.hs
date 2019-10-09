-- | This module implements a Foldl-based interface for arrow computations
-- compatible with the <https://hackage.haskell.org/package/foldl foldl
-- library>. Use 'generalizeA' and 'generalizeM' to convert folds to
-- 'FoldA'. This is the most general way to repeat a PTask over some an input
-- (list, array, stream, etc.).
--
-- This API is still experimental and might be subject to changes in the future

module Porcupine.Foldl
  ( module System.TaskPipeline.Repetition.Foldl )
where

import System.TaskPipeline.Repetition.Foldl
