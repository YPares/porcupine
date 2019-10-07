{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE GADTs               #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module System.TaskPipeline.Options
  ( -- * API
    getOptions
  , getOption
  , optionsVirtualFile
  , optionVirtualFile
  -- * Re-exports from docrecords
  , DocRec, Rec(..), (^^.), (^^?), (^^?!), (=:)
  , PathWithType(..)
  , docField
  , pattern FV
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

import           Prelude                               hiding (id, (.))


-- | Field Value. Allows you to directly pattern match on the output of
-- 'getOptions'/'loadData'
pattern FV :: a -> DocField (s:|:a)
pattern FV v <- DocField _ (Right (Field v))

-- | Creates a 'VirtualFile' from a default set of options (as a DocRec). To be
-- used with 'loadData'.
optionsVirtualFile
  :: forall rs. (Typeable rs, RecordUsableWithCLI rs)
  => [LocationTreePathItem]  -- ^ The path for the options in the LocationTree
  -> DocRec rs               -- ^ The DocRec containing the fields with their
                             -- docs and default values
  -> BidirVirtualFile (DocRec rs)
optionsVirtualFile path defOpts =
  withEmbeddedValue defOpts $
    bidirVirtualFile path $
         someBidirSerial (OptionsSerial id id :: OptionsSerial (DocRec rs))
      <> someBidirSerial YAMLSerial

-- | Just like 'optionsVirtualFile', but for a single field
optionVirtualFile
  :: (KnownSymbol s, Typeable t, ToJSON t, FieldFromCLI ('[s] :|: t))
  => [LocationTreePathItem] -- ^ The path for the option field in the LocationTree
  -> DocField ('[s] :|: t)  -- ^ The field, usually with default value (created
                            -- with 'docField')
  -> BidirVirtualFile t
optionVirtualFile path field =
  dimap (field =:) (^^?! field) $
    optionsVirtualFile path (field :& RNil)

-- | Add a set of options (as a DocRec) to the 'LocationTree', in order to
-- expose them to the user, and returns the final values of these options
getOptions
  :: (LogThrow m, Typeable rs, RecordUsableWithCLI rs)
  => [LocationTreePathItem]  -- ^ The path for the options in the LocationTree
  -> DocRec rs               -- ^ The DocRec containing the fields with their
                             -- docs and default values
  -> PTask m () (DocRec rs)  -- ^ A PTask that returns the new options values,
                             -- overriden by the user
getOptions path = loadData . optionsVirtualFile path

-- | Just like 'getOptions', but for a single field.
getOption
  :: (LogThrow m, KnownSymbol s, Typeable t, ToJSON t, FieldFromCLI ('[s] :|: t))
  => [LocationTreePathItem]  -- ^ The path for the option field in the LocationTree
  -> DocField ('[s] :|: t)   -- ^ The field (created with 'docField')
  -> PTask m () t            -- ^ A PTask that returns the new option,
                             -- overriden by the user
getOption path = loadData . optionVirtualFile path
