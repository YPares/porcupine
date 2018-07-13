{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -Wall                   #-}

module Data.Locations.Localizable
  ( Localizable(..)
  , localize
  , localize'
  , HasRunContext(..), MonadReader(..)
  , onOutputDir, onInputDir
  ) where

import           Data.Locations.Loc
import           System.RunContext

import           Control.Lens
import           Data.Proxy         (Proxy (Proxy))


-- | @a@ should be some type of artifact. This class gives a location where it
-- should be stored it, before this artifact even exists, provided we have some
-- context @r@
class Localizable a r where
  -- | Gives a relative location to some localizable object
  toRelLoc :: Proxy a -> r -> FilePath

localize
  :: forall a r m.
     (Localizable a r, MonadReader r m, HasRunContext r)
  => Proxy a
  -> m Loc
localize _ = do
  fullCtx <- ask
  return $ fullCtx^.runContext.curRoot </> toRelLoc (Proxy @a) fullCtx

-- | Just like 'localize' but takes an already existing value, not a 'Proxy'
localize'
  :: forall a r m.
     (Localizable a r, MonadReader r m, HasRunContext r)
  => a
  -> m Loc
localize' _ = localize (Proxy :: Proxy a)
