{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE FlexibleContexts #-}

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


class ( Eq l, FromJSON l, ToJSON l
      , Store l, ContentHashable Identity l )
   => Location l where
  

class (Location (LocationOf m))
   => LocationAccessor m where
  type LocationOf m :: *

  locExists :: LocationOf m -> m Bool

  writeBSS :: Loc -> BSS.ByteString m r -> m r

  readBSS :: Loc -> (BSS.ByteString m () -> m b) -> m b
  
