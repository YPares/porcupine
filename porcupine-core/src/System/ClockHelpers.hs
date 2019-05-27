module System.ClockHelpers
  ( Clock(..)
  , TimeSpec(..)
  , diffTimeSpec
  , getTime
  , showTimeSpec
  , clockM
  )where

import Control.Monad.IO.Class
import System.Clock


showTimeSpec :: TimeSpec -> String
showTimeSpec (TimeSpec s ns) = show s ++ "s," ++ show ns ++ "ns"
  -- Ugly, change that

clockM :: (MonadIO m) => m a -> m (a, TimeSpec)
clockM act = f <$> time <*> act <*> time
  where
    time = liftIO $ getTime Realtime
    f start res end = (res, end `diffTimeSpec` start)
