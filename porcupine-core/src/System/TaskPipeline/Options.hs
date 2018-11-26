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

import           Prelude                               hiding (id, (.))

import           Control.Lens
import           Data.Aeson
import           Data.DocRecord
import           Data.DocRecord.OptParse
import           Data.Locations.LocationMonad
import           Data.Locations.SerializationMethod
import           Data.Locations.VirtualFile
import           Data.Monoid                           (Last (..))
import           Data.Typeable
import           GHC.TypeLits                          (KnownSymbol)
import           Katip
import qualified Streaming.Prelude                     as S
import           System.TaskPipeline.PTask
import           System.TaskPipeline.VirtualFileAccess


-- | Add a set of options (as a DocRec) to the 'LocationTree', in order to
-- expose them to the user, and returns the final values of these options
getOptions
  :: (LocationMonad m, KatipContext m, Typeable rs, RecordUsableWithCLI rs)
  => [LocationTreePathItem]  -- ^ The path for the options in the LocationTree
  -> DocRec rs               -- ^ The DocRec containing the fields with their
                             -- docs and default values
  -> PTask m () (DocRec rs)  -- ^ A PTask that returns the new options values,
                             -- overriden by the user
getOptions path defOpts =
      arr (\_ -> S.yield ([] :: [Int], error "getOptions: THIS IS VOID"))
  >>> accessVirtualFile [] vfile
  >>> streamHeadTask
  >>> arr post
  where
    defOpts' = Last $ Just defOpts
    post (Last Nothing)  = defOpts
    post (Last (Just x)) = x
    vfile = bidirVirtualFile path $
            serials & serialWriters . serialWritersToOutputFile .~ mempty
            -- We remove all the writers, so if options are read from a file
            -- this file isn't overwritten
    serials =
      someBidirSerial (DocRecSerial defOpts' post (Last . Just))
      -- TODO: merge properly the docrecs here instead of just using the last
      -- one
      <>
      someBidirSerial JSONSerial

-- | Just like 'getOptions', but for a single field.
getOption
  :: ( LocationMonad m, KatipContext m
     , KnownSymbol s, Typeable t, ToJSON t, FieldFromCLI ('[s] :|: t))
  => [LocationTreePathItem]  -- ^ The path for the option field in the LocationTree
  -> DocField ('[s] :|: t)   -- ^ The field (created with 'docField')
  -> PTask m () t            -- ^ A PTask that returns the new option,
                             -- overriden by the user
getOption path field =
  getOptions path record >>> arr (^^?! field)
  where
    record = field :& RNil
