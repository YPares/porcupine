-- | The top level module for porcupine. Suitable for examples. Prefer
-- selectively importing the needed Porcupine.* modules in bigger applications
-- that define serials and tasks in separate files.

module Porcupine
  ( module Porcupine.Serials
  , module Porcupine.VFiles
  , module Porcupine.Tasks
  , module Porcupine.Run )
where

import           Porcupine.Run
import           Porcupine.Serials
import           Porcupine.Tasks
import           Porcupine.VFiles
