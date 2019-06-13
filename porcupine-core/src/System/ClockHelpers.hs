{-# OPTIONS_GHC -fno-warn-orphans       #-}

module System.ClockHelpers
  ( Clock(..)
  , TimeSpec(..)
  , diffTimeSpec
  , getTime
  , showTimeSpec
  , clockM
  ) where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Katip
import           System.Clock


clockM :: (MonadIO m) => m a -> m (a, TimeSpec)
clockM act = f <$> time <*> act <*> time
  where
    time = liftIO $ getTime Realtime
    f start res end = (res, end `diffTimeSpec` start)

showTimeSpec :: TimeSpec -> String
showTimeSpec ts = show (fromIntegral (toNanoSecs ts) / (10**9) :: Double) ++ "s"

-- To be able to use TimeSpec as part of katip contexts (katipAddContext)
instance ToJSON TimeSpec
instance ToObject TimeSpec
instance LogItem TimeSpec where
  payloadKeys v _ | v >= V1   = AllKeys
                  | otherwise = SomeKeys []
