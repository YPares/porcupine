module System.TaskPipeline.Tasks
  ( module System.TaskPipeline.ATask
  , module System.TaskPipeline.ResourceTree
  , module System.TaskPipeline.Tasks.LayeredAccess
  , module System.TaskPipeline.Tasks.Options
  , module System.TaskPipeline.Tasks.Repetition
  , module Data.Locations.LogAndErrors
  , PTask
  ) where

import           Data.Locations.LogAndErrors
import           System.TaskPipeline.ATask
import           System.TaskPipeline.ResourceTree
import           System.TaskPipeline.Tasks.LayeredAccess
import           System.TaskPipeline.Tasks.Options
import           System.TaskPipeline.Tasks.Repetition

-- | An alias for 'ATask'
type PTask m = ATask m
