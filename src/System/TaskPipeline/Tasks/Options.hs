{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# OPTIONS_GHC -Wall #-}

-- IMPORTANT! Some inferences with DocRecs (equational constraints) don't work
-- if we don't activate GADTs

module System.TaskPipeline.Tasks.Options
  ( getOptions
  , DocRec, Rec(..), (^^.), (^^?), (^^?!)  -- re-exporting some operators from
                                           -- DocRecords
  ) where

import           Control.Lens
import           Data.DocRecord
import           Data.DocRecord.OptParse
import           Data.Locations
import           Data.Typeable
import           Katip
import           System.TaskPipeline.ATask
import           System.TaskPipeline.Resource


castAs :: (Typeable a, Typeable a') => a -> a' -> Maybe a
castAs _ = cast

-- | Add a set of options (as a DocRec) to the 'LocationTree', in order to
-- expose them to the user, and returns the final values of these options
getOptions
  :: (LocationMonad m, KatipContext m, Typeable rs, RecordUsableWithCLI rs)
  => [LocationTreePathItem]  -- ^ The path for the options in the LocationTree
  -> DocRec rs               -- ^ The DocRec containing the fields with their
                             -- docs and default values
  -> ATask m PipelineResource () (DocRec rs)  -- ^ An ATask that returns the new
                                              -- options values, overriden by
                                              -- the user
getOptions path defOpts =
  liftToATask (init path)
     (Identity $
       file (last path) (PRscOptions $ RecOfOptions defOpts)) run
  where
    run _ (Identity (PRscVirtualFile _)) = throwWithPrefix $
      "Reading options straight from a file is not supported yet"
    run _ (Identity (PRscOptions (RecOfOptions newDocRec))) = do
      case castAs defOpts newDocRec of
        Nothing -> throwWithPrefix $
          "The DocRec received isn't of the same type as the input one\n"
          <> "      This might be caused by a duplicated Virtual Option file in the same location\n"
          <> "      To solve this case you need to change the Option virtual file name"
        Just newOpts -> return newOpts
    run _ (Identity PRscNothing) = return defOpts
    run _ _ = throwWithPrefix $
      "The DocRec of options awaited isn't present in the LocationTree"
