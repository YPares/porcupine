{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

module System.TaskPipeline.Tasks.Options
  ( getOptions
  , DocRec, Rec(..), (^^.), (^^?), (^^?!)  -- re-exporting some operators from
                                           -- DocRecords
  ) where

import           Prelude                                 hiding (id, (.))

import           Data.DocRecord
import           Data.DocRecord.OptParse
import           Data.Locations.LocationMonad
import           Data.Locations.SerializationMethod
import           Data.Locations.VirtualFile
import           Data.Monoid                             (Last (..))
import           Data.Typeable
import           Katip
import           System.TaskPipeline.ATask
import           System.TaskPipeline.Resource
import           System.TaskPipeline.Tasks.LayeredAccess


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
getOptions path defOpts = arr (const defOpts') >>> accessVirtualFile vfile >>> arr post
  where
    defOpts' = Last $ Just defOpts
    post (Last Nothing)  = defOpts
    post (Last (Just x)) = x
    vfile = bidirVirtualFile path $ someBidirSerial $
      DocRecSerial defOpts' (\(Last (Just x)) -> x) (Last . Just)
