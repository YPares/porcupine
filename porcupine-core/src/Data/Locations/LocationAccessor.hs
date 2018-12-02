{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Locations.LocationAccessor where

import Control.Funflow.ContentHashable
import Control.Monad.IO.Unlift
import Data.Aeson
import Data.Locations.Loc
import Data.Locations.LocationMonad
import Data.Functor.Identity
import Data.Store                      (Store)
import qualified Data.ByteString.Streaming             as BSS
import qualified Data.ByteString.Lazy                  as LBS


-- class ( Eq l, FromJSON l, ToJSON l
--       , Store l, ContentHashable Identity l )
--    => Location l where
  

-- class (Location l)
--    => LocationAccessor l m where
--   locExists :: l m -> IO Bool

--   writeBSS :: l m -> BSS.ByteString IO r -> IO r

--   readBSS :: l m -> (BSS.ByteString IO () -> IO b) -> IO b
  
