{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# OPTIONS_GHC -Wall #-}

-- IMPORTANT! Some inferences with DocRecs (equational constraints) don't work
-- if we don't activate GADTs

module System.TaskPipeline.Tasks.Options
  ( getOptionsTask
  , DocRec, Rec(..), (^^.), (^^?), (^^?!)  -- re-exporting some operators from
                                           -- DocRecords
  ) where

import           Control.Lens
import           Data.DocRecord
import           Data.DocRecord.OptParse
import           Data.Locations
import           Data.Typeable
import           System.TaskPipeline.ATask
import           System.TaskPipeline.Resource


castAs :: (Typeable a, Typeable a') => a -> a' -> Maybe a
castAs _ = cast

-- | Add a set of options (as a DocRec) to the 'LocationTree', in order to
-- expose them to the user, and returns the final values of these options
getOptionsTask
  :: (LocationMonad m, Typeable rs, RecordUsableWithCLI rs)
  => [LocationTreePathItem]  -- ^ The folder path in the LocationTree
  -> LTPIAndSubtree a        -- ^ The virtual file that will "contain" the options
  -> String                  -- ^ The task name (for error messages)
  -> DocRec rs               -- ^ The DocRec containing the fields with their
                             -- docs and default values
  -> ATask m PipelineResource () (DocRec rs)  -- ^ An ATask that returns the new
                                              -- options values, overriden by
                                              -- the user
getOptionsTask path filep taskName defOpts =
  liftToATask path
     (Identity $
       filep & traversed .~ PRscOptions (RecOfOptions defOpts)) taskName run
  where
    run _ (Identity (PRscVirtualFile _)) = throwM $ TaskRunError $
      taskName ++ ": Reading options straight from a file is not supported yet"
    run _ (Identity (PRscOptions (RecOfOptions newDocRec))) =
      case castAs defOpts newDocRec of
        Nothing -> throwM $ TaskRunError $
          taskName ++ ": The DocRec received isn't of the same type as the input one"
        Just newOpts -> return newOpts
    run _ (Identity PRscNothing) = return defOpts
    run _ _ = throwM $ TaskRunError $
      taskName ++ ": The DocRec of options awaited isn't present in the LocationTree"
