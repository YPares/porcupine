{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module System.TaskPipeline.Options
  ( -- * API
    getOptions
  , getOption
  -- * Re-exports from docrecords
  , DocRec, Rec(..), (^^.), (^^?), (^^?!)
  , PathWithType(..)
  , docField
  ) where

import           Data.Aeson
import           Data.DocRecord
import           Data.DocRecord.OptParse
import           Data.Locations.SerializationMethod
import           Data.Locations.VirtualFile
import           Data.Typeable
import           GHC.TypeLits                          (KnownSymbol)
import           System.TaskPipeline.PTask
import           System.TaskPipeline.VirtualFileAccess

import           Prelude                               hiding (id)


-- | Add a set of options (as a DocRec) to the 'LocationTree', in order to
-- expose them to the user, and returns the final values of these options
getOptions
  :: (LogThrow m, Typeable rs, RecordUsableWithCLI rs)
  => [LocationTreePathItem]  -- ^ The path for the options in the LocationTree
  -> DocRec rs               -- ^ The DocRec containing the fields with their
                             -- docs and default values
  -> PTask m () (DocRec rs)  -- ^ A PTask that returns the new options values,
                             -- overriden by the user
getOptions path defOpts = loadData $
  bidirVirtualFile path $
    someBidirSerial (DocRecSerial defOpts id id) <> someBidirSerial YAMLSerial

-- | Just like 'getOptions', but for a single field.
getOption
  :: (LogThrow m, KnownSymbol s, Typeable t, ToJSON t, FieldFromCLI ('[s] :|: t))
  => [LocationTreePathItem]  -- ^ The path for the option field in the LocationTree
  -> DocField ('[s] :|: t)   -- ^ The field (created with 'docField')
  -> PTask m () t            -- ^ A PTask that returns the new option,
                             -- overriden by the user
getOption path field =
  getOptions path record >>> arr (^^?! field)
  where
    record = field :& RNil
